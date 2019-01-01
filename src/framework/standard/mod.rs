pub mod help_commands;
pub mod macros {
    pub use command_attr::{initialize, command, group, group_options, help};
}

mod args;
mod configuration;
mod parse;
mod structures;

pub use self::args::{Args, Delimiter, Error as ArgError, Iter};
pub use self::configuration::Configuration;
pub use self::structures::*;


use self::parse::*;
use super::Framework;
use crate::client::Context;
use crate::model::channel::Message;
use std::sync::Arc;
use threadpool::ThreadPool;

/// An enum representing all possible fail conditions under which a command won't
/// be executed.
#[derive(Debug)]
pub enum DispatchError {
    /// When a custom function check has failed.
    CheckFailed,
    /// When the requested command is disabled in bot configuration.
    CommandDisabled(String),
    /// When the requested command can only be used in a direct message or group
    /// channel.
    OnlyForDM,
    /// When the requested command can only be ran in guilds, or the bot doesn't
    /// support DMs.
    OnlyForGuilds,
    /// When the requested command can only be used by bot owners.
    OnlyForOwners,
    /// When the requested command requires one role.
    LackingRole,
    /// When there are too few arguments.
    NotEnoughArguments { min: u8, given: usize },
    /// When there are too many arguments.
    TooManyArguments { max: u8, given: usize },
}

pub type DispatchHook = Fn(&mut Context, &Message, DispatchError) + Send + Sync + 'static;
type BeforeHook = Fn(&mut Context, &Message, &str) -> bool + Send + Sync + 'static;
type AfterHook = Fn(&mut Context, &Message, &str, Result<(), CommandError>) + Send + Sync + 'static;
type UnrecognisedHook = Fn(&mut Context, &Message, &str) + Send + Sync + 'static;
type NormalMessageHook = Fn(&mut Context, &Message) + Send + Sync + 'static;
type PrefixOnlyHook = Fn(&mut Context, &Message) + Send + Sync + 'static;

/// A utility for easily managing dispatches to commands.
///
/// Refer to the [module-level documentation] for more information.
///
/// [module-level documentation]: index.html
#[derive(Default)]
pub struct StandardFramework {
    groups: Vec<&'static CommandGroup>,
    before: Option<Arc<BeforeHook>>,
    after: Option<Arc<AfterHook>>,
    dispatch: Option<Arc<DispatchHook>>,
    unrecognised_command: Option<Arc<UnrecognisedHook>>,
    normal_message: Option<Arc<NormalMessageHook>>,
    prefix_only: Option<Arc<PrefixOnlyHook>>,
    config: Configuration,
    help: Option<&'static HelpCommand>,
    /// Whether the framework has been "initialized".
    ///
    /// The framework is initialized once one of the following occurs:
    ///
    /// - configuration has been set;
    /// - a command handler has been set;
    /// - a command check has been set.
    ///
    /// This is used internally to determine whether or not - in addition to
    /// dispatching to the [`EventHandler::message`] handler - to have the
    /// framework check if a [`Event::MessageCreate`] should be processed by
    /// itself.
    ///
    /// [`EventHandler::message`]: ../../client/trait.EventHandler.html#method.message
    /// [`Event::MessageCreate`]: ../../model/event/enum.Event.html#variant.MessageCreate
    pub initialized: bool,
}

impl StandardFramework {
    #[inline]
    pub fn new() -> Self {
        StandardFramework::default()
    }

    /// Configures the framework, setting non-default values. All fields are
    /// optional. Refer to [`Configuration::default`] for more information on
    /// the default values.
    ///
    /// # Examples
    ///
    /// Configuring the framework for a [`Client`], [allowing whitespace between prefixes], and setting the [`prefix`] to `"~"`:
    ///
    /// ```rust,no_run
    /// # use serenity::prelude::EventHandler;
    /// # struct Handler;
    /// # impl EventHandler for Handler {}
    /// use serenity::Client;
    /// use serenity::framework::StandardFramework;
    /// use std::env;
    ///
    /// let token = env::var("DISCORD_TOKEN").unwrap();
    /// let mut client = Client::new(&token, Handler).unwrap();
    /// client.with_framework(StandardFramework::new()
    ///     .configure(|c| c
    ///         .with_whitespace(true)
    ///         .prefix("~")));
    /// ```
    ///
    /// [`Client`]: ../../client/struct.Client.html
    /// [`Configuration::default`]: struct.Configuration.html#method.default
    /// [`prefix`]: struct.Configuration.html#method.prefix
    /// [allowing whitespace between prefixes]: struct.Configuration.html#method.with_whitespace
    pub fn configure<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut Configuration) -> &mut Configuration,
    {
        f(&mut self.config);

        self
    }

    fn should_fail(
        &mut self,
        ctx: &mut Context,
        msg: &Message,
        args: &mut Args,
        command: &'static CommandOptions,
        group: &'static GroupOptions,
    ) -> Option<DispatchError> {
        if let Some(min) = command.min_args {
            if args.len() < min as usize {
                return Some(DispatchError::NotEnoughArguments {
                    min,
                    given: args.len(),
                });
            }
        }

        if let Some(max) = command.max_args {
            if args.len() > max as usize {
                return Some(DispatchError::TooManyArguments {
                    max,
                    given: args.len(),
                });
            }
        }

        if (group.owner_privilege || command.owner_privilege)
            && self.config.owners.contains(&msg.author.id)
        {
            return None;
        }

        if group.owners_only || command.owners_only {
            return Some(DispatchError::OnlyForOwners);
        }

        if self.config.disabled_commands.contains(command.names[0]) {
            return Some(DispatchError::CommandDisabled(command.names[0].to_string()));
        }

        if (group.only == OnlyIn::Dm || command.only_in == OnlyIn::Dm) && !msg.is_private() {
            return Some(DispatchError::OnlyForDM);
        }

        if (!self.config.allow_dm
            || (group.only == OnlyIn::Guild || command.only_in == OnlyIn::Guild))
            && msg.is_private()
        {
            return Some(DispatchError::OnlyForGuilds);
        }

        for Check(check) in command.checks {
            if !check(ctx, msg, args, command) {
                return Some(DispatchError::CheckFailed);
            }
        }

        for Check(check) in group.checks {
            if !check(ctx, msg, args, command) {
                return Some(DispatchError::CheckFailed);
            }
        }

        None
    }

    /// Adds a group which can organize several related commands.
    /// Groups are taken into account when using
    /// `serenity::framework::standard::help_commands`.
    pub fn group(mut self, group: &'static CommandGroup) -> Self {
        self.groups.push(group);

        self.initialized = true;

        self
    }

    /// Specify the function that's called in case a command wasn't executed for one reason or
    /// another.
    ///
    /// DispatchError represents all possible fail conditions.
    ///
    /// # Examples
    ///
    /// Making a simple argument error responder:
    ///
    /// ```rust,no_run
    /// # use serenity::prelude::*;
    /// # struct Handler;
    /// #
    /// # impl EventHandler for Handler {}
    /// # let mut client = Client::new("token", Handler).unwrap();
    /// use serenity::framework::standard::DispatchError::{NotEnoughArguments,
    /// TooManyArguments};
    /// use serenity::framework::StandardFramework;
    ///
    /// client.with_framework(StandardFramework::new()
    ///     .on_dispatch_error(|_, msg, error| {
    ///         match error {
    ///             NotEnoughArguments { min, given } => {
    ///                 let s = format!("Need {} arguments, but only got {}.", min, given);
    ///
    ///                 let _ = msg.channel_id.say(&s);
    ///             },
    ///             TooManyArguments { max, given } => {
    ///                 let s = format!("Max arguments allowed is {}, but got {}.", max, given);
    ///
    ///                 let _ = msg.channel_id.say(&s);
    ///             },
    ///             _ => println!("Unhandled dispatch error."),
    ///         }
    ///     }));
    /// ```
    pub fn on_dispatch_error<F>(mut self, f: F) -> Self
    where
        F: Fn(&mut Context, &Message, DispatchError) + Send + Sync + 'static,
    {
        self.dispatch = Some(Arc::new(f));

        self
    }

    /// Specify the function to be called prior to every command's execution.
    /// If that function returns true, the command will be executed.
    ///
    /// # Examples
    ///
    /// Using `before` to log command usage:
    ///
    /// ```rust,no_run
    /// # use serenity::prelude::*;
    /// # struct Handler;
    /// #
    /// # impl EventHandler for Handler {}
    /// # let mut client = Client::new("token", Handler).unwrap();
    /// #
    /// use serenity::framework::StandardFramework;
    ///
    /// client.with_framework(StandardFramework::new()
    ///     .before(|ctx, msg, cmd_name| {
    ///         println!("Running command {}", cmd_name);
    ///         true
    ///     }));
    /// ```
    ///
    /// Using before to prevent command usage:
    ///
    /// ```rust,no_run
    /// # use serenity::prelude::*;
    /// # struct Handler;
    /// #
    /// # impl EventHandler for Handler {}
    /// # let mut client = Client::new("token", Handler).unwrap();
    /// #
    /// use serenity::framework::StandardFramework;
    ///
    /// client.with_framework(StandardFramework::new()
    ///     .before(|ctx, msg, cmd_name| {
    ///         if let Ok(channel) = msg.channel_id.to_channel(&ctx) {
    ///             //  Don't run unless in nsfw channel
    ///             if !channel.is_nsfw() {
    ///                 return false;
    ///             }
    ///         }
    ///
    ///         println!("Running command {}", cmd_name);
    ///
    ///         true
    ///     }));
    /// ```
    ///
    pub fn before<F>(mut self, f: F) -> Self
    where
        F: Fn(&mut Context, &Message, &str) -> bool + Send + Sync + 'static,
    {
        self.before = Some(Arc::new(f));

        self
    }

    /// Specify the function to be called after every command's execution.
    /// Fourth argument exists if command returned an error which you can handle.
    ///
    /// # Examples
    ///
    /// Using `after` to log command usage:
    ///
    /// ```rust,no_run
    /// # use serenity::prelude::*;
    /// # struct Handler;
    /// #
    /// # impl EventHandler for Handler {}
    /// # let mut client = Client::new("token", Handler).unwrap();
    /// #
    /// use serenity::framework::StandardFramework;
    ///
    /// client.with_framework(StandardFramework::new()
    ///     .after(|ctx, msg, cmd_name, error| {
    ///         //  Print out an error if it happened
    ///         if let Err(why) = error {
    ///             println!("Error in {}: {:?}", cmd_name, why);
    ///         }
    ///     }));
    /// ```
    pub fn after<F>(mut self, f: F) -> Self
    where
        F: Fn(&mut Context, &Message, &str, Result<(), CommandError>) + Send + Sync + 'static,
    {
        self.after = Some(Arc::new(f));

        self
    }

    /// Specify the function to be called if no command could be dispatched.
    ///
    /// # Examples
    ///
    /// Using `unrecognised_command`:
    ///
    /// ```rust,no_run
    /// # use serenity::prelude::*;
    /// # struct Handler;
    /// #
    /// # impl EventHandler for Handler {}
    /// # let mut client = Client::new("token", Handler).unwrap();
    /// #
    /// use serenity::framework::StandardFramework;
    ///
    /// client.with_framework(StandardFramework::new()
    ///     .unrecognised_command(|ctx, msg, unrecognised_command_name| { }));
    /// ```
    pub fn unrecognised_command<F>(mut self, f: F) -> Self
    where
        F: Fn(&mut Context, &Message, &str) + Send + Sync + 'static,
    {
        self.unrecognised_command = Some(Arc::new(f));

        self
    }

    /// Specify the function to be called if a message contains no command.
    ///
    /// # Examples
    ///
    /// Using `message_without_command`:
    ///
    /// ```rust,no_run
    /// # use serenity::prelude::*;
    /// # struct Handler;
    /// #
    /// # impl EventHandler for Handler {}
    /// # let mut client = Client::new("token", Handler).unwrap();
    /// #
    /// use serenity::framework::StandardFramework;
    ///
    /// client.with_framework(StandardFramework::new()
    ///     .normal_message(|ctx, msg| { }));
    /// ```
    pub fn normal_message<F>(mut self, f: F) -> Self
    where
        F: Fn(&mut Context, &Message) + Send + Sync + 'static,
    {
        self.normal_message = Some(Arc::new(f));

        self
    }

    /// Sets what code should be executed when a user sends `(prefix)help`.
    ///
    /// If a command named `help` in a group was set, then this takes precedence first.
    ///
    /// [`command`]: #method.command
    pub fn help(mut self, h: &'static HelpCommand) -> Self {
        self.help = Some(h);

        self
    }
}

impl Framework for StandardFramework {
    fn dispatch(&mut self, mut ctx: Context, msg: Message, threadpool: &ThreadPool) {
        // Ignore bots/webhooks.
        if msg.author.bot || msg.webhook_id.is_some() {
            return;
        }

        let (prefix, rest) = parse_prefix(&mut ctx, &msg, &self.config);

        if prefix != Prefix::None && rest.trim().is_empty() {
            if let Some(prefix_only) = &self.prefix_only {
                let prefix_only = Arc::clone(&prefix_only);
                let msg = msg.clone();

                threadpool.execute(move || {
                    prefix_only(&mut ctx, &msg);
                });
            }

            return;
        }

        if prefix == Prefix::None && !(self.config.no_dm_prefix && msg.is_private()) {
            if let Some(normal) = &self.normal_message {
                let normal = Arc::clone(&normal);
                let msg = msg.clone();

                threadpool.execute(move || {
                    normal(&mut ctx, &msg);
                });
            }

            return;
        }

        let invoke = match parse_command(
            rest,
            prefix,
            &self.groups,
            &self.config,
            self.help.is_some(),
        ) {
            Ok(i) => i,
            Err(Some(unreg)) => {
                if let Some(unrecognised_command) = &self.unrecognised_command {
                    let unrecognised_command = Arc::clone(&unrecognised_command);
                    let mut ctx = ctx.clone();
                    let msg = msg.clone();
                    let unreg = unreg.to_string();
                    threadpool.execute(move || {
                        unrecognised_command(&mut ctx, &msg, &unreg);
                    });
                }

                if let Some(normal) = &self.normal_message {
                    let normal = Arc::clone(&normal);
                    let msg = msg.clone();

                    threadpool.execute(move || {
                        normal(&mut ctx, &msg);
                    });
                }

                return;
            }
            Err(None) => {
                if let Some(normal) = &self.normal_message {
                    let normal = Arc::clone(&normal);
                    let msg = msg.clone();
                    threadpool.execute(move || {
                        normal(&mut ctx, &msg);
                    });
                }

                return;
            }
        };

        match invoke {
            Invoke::Help {
                prefix: _prefix,
                args,
            } => {
                let args = Args::new(args, &self.config.delimiters);

                let before = self.before.clone();
                let after = self.after.clone();
                let groups = self.groups.clone();
                let msg = msg.clone();
                let name = "help";

                // `parse_command` promises to never return a help invocation if `StandardFramework::help` is `None`.
                let help = self.help.clone().unwrap();

                threadpool.execute(move || {
                    if let Some(before) = before {
                        if !before(&mut ctx, &msg, name) {
                            return;
                        }
                    }

                    let res = (help.fun)(&mut ctx, &msg, args, help.options, &groups);

                    if let Some(after) = after {
                        after(&mut ctx, &msg, name, res);
                    }
                });
            }
            Invoke::Command {
                prefix: _prefix,
                gprefix: _gprefix,
                command,
                group,
                args,
            } => {
                let mut args = Args::new(args, &self.config.delimiters);

                if let Some(error) =
                    self.should_fail(&mut ctx, &msg, &mut args, &command.options, &group.options)
                {
                    if let Some(dispatch) = &self.dispatch {
                        dispatch(&mut ctx, &msg, error);
                    }

                    return;
                }

                let before = self.before.clone();
                let after = self.after.clone();
                let msg = msg.clone();
                let name = &command.options.names[0];
                threadpool.execute(move || {
                    if let Some(before) = before {
                        if !before(&mut ctx, &msg, name) {
                            return;
                        }
                    }

                    let res = (command.fun)(&mut ctx, &msg, args);

                    if let Some(after) = after {
                        after(&mut ctx, &msg, name, res);
                    }
                });
            }
        }
    }
}
