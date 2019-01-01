use std::fmt;
use crate::client::Context;
use crate::model::channel::Message;
use super::Args;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OnlyIn {
    Dm,
    Guild,
    None,
}

/// A precondition to a command before its execution.
pub struct Check(pub fn(&mut Context, &Message, &mut Args, &CommandOptions) -> bool);

impl PartialEq for Check {
    fn eq(&self, other: &Check) -> bool {
        (self.0 as usize) == (other.0 as usize)
    }
}

impl fmt::Debug for Check {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad("Check(<fn>)")
    }
}

impl Clone for Check {
    fn clone(&self) -> Self {
        Check(self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct CommandOptions {
    pub checks: &'static [Check],
    pub names: &'static [&'static str],
    pub desc: Option<&'static str>,
    pub usage: Option<&'static str>,
    pub min_args: Option<u8>,
    pub max_args: Option<u8>,
    pub allowed_roles: &'static [&'static str],
    pub help_available: bool,
    pub only_in: OnlyIn,
    pub owners_only: bool,
    pub owner_privilege: bool,
    pub sub: &'static [&'static Command],
}

#[derive(Debug, PartialEq)]
pub struct GroupOptions {
    pub prefixes: &'static [&'static str],
    pub only: OnlyIn,
    pub owners_only: bool,
    pub owner_privilege: bool,
    pub help_available: bool,
    pub allowed_roles: &'static [&'static str],
    pub checks: &'static [Check],
    pub default_command: Option<&'static Command>,
    pub description: Option<&'static str>,
}

#[derive(Debug, Clone)]
pub struct CommandError(pub String);

impl<T: fmt::Display> From<T> for CommandError {
    #[inline]
    fn from(d: T) -> Self {
        CommandError(d.to_string())
    }
}

pub type CommandResult = ::std::result::Result<(), CommandError>;

pub type CommandFn = fn(&mut Context, &Message, Args) -> CommandResult;

pub struct Command {
    pub fun: CommandFn,
    pub options: &'static CommandOptions,
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Command")
            .field("options", &self.options)
            .finish()
    }
}

impl PartialEq for Command {
    #[inline]
    fn eq(&self, other: &Command) -> bool {
        (self.fun as usize == other.fun as usize) && (self.options == other.options)
    }
}

pub type HelpCommandFn = fn(
    &mut Context,
    &Message,
    Args,
    &'static HelpOptions,
    &[&'static CommandGroup],
) -> CommandResult;

pub struct HelpCommand {
    pub fun: HelpCommandFn,
    pub options: &'static HelpOptions,
}

impl fmt::Debug for HelpCommand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("HelpCommand")
            .field("fun", &"<function>")
            .field("options", &self.options)
            .finish()
    }
}

impl PartialEq for HelpCommand {
    #[inline]
    fn eq(&self, other: &HelpCommand) -> bool {
        (self.fun as usize == other.fun as usize) && (self.options == other.options)
    }
}

/// Describes the behaviour the help-command shall execute once it encounters
/// a command which the user or command fails to meet following criteria :
/// Lacking required permissions to execute the command.
/// Lacking required roles to execute the command.
/// The command can't be used in the current channel (as in `DM only` or `guild only`).
#[derive(PartialEq, Debug)]
pub enum HelpBehaviour {
    /// Strikes a command by applying `~~{command_name}~~`.
    Strike,
    /// Does not list a command in the help-menu.
    Hide,
    /// The command will be displayed, hence nothing will be done.
    Nothing,
}

#[derive(Debug, PartialEq)]
pub struct HelpOptions {
    pub suggestion_text: &'static str,
    pub no_help_available_text: &'static str,
    pub usage_label: &'static str,
    pub usage_sample_label: &'static str,
    pub ungrouped_label: &'static str,
    pub description_label: &'static str,
    pub grouped_label: &'static str,
    pub aliases_label: &'static str,
    pub guild_only_text: &'static str,
    pub dm_only_text: &'static str,
    pub dm_and_guild_text: &'static str,
    pub available_text: &'static str,
    pub command_not_found_text: &'static str,
    pub individual_command_tip: &'static str,
    pub striked_commands_tip_in_dm: Option<&'static str>,
    pub striked_commands_tip_in_guild: Option<&'static str>,
    pub group_prefix: &'static str,
    pub lacking_role: HelpBehaviour,
    pub lacking_permissions: HelpBehaviour,
    pub wrong_channel: HelpBehaviour,
    pub embed_error_colour: u64,
    pub embed_success_colour: u64,
    pub max_levenshtein_distance: usize,
}

#[derive(Debug)]
pub struct CommandGroup {
    pub name: &'static str,
    pub options: &'static GroupOptions,
    pub commands: &'static [&'static Command],
    pub sub: &'static [&'static CommandGroup],
}
