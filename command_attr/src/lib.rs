// It has become too of a burden for the `quote!` macro.
#![recursion_limit = "128"]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse::{Error, Parse, ParseStream, Result},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    Ident, Lit, ReturnType, Type,
};

use std::cell::RefCell;

pub(crate) mod attributes;
pub(crate) mod consts;
pub(crate) mod structures;
pub(crate) mod util;

use self::attributes::*;
use self::consts::*;
use self::structures::*;
use self::util::*;

thread_local! {
    pub(crate) static CRATE_NAME: RefCell<String> = RefCell::new(String::new());
}

#[proc_macro]
pub fn initialize(input: TokenStream) -> TokenStream {
    let name = parse_macro_input!(input as Ident);
    let mut name = name.to_string();

    if name == "_crate" {
        // Discard the underscore.
        name.remove(0);
    }

    CRATE_NAME.with(|cn| cn.replace(name));

    TokenStream::new()
}

fn validate_declaration(fun: &mut CommandFun, is_help: bool) -> Result<()> {
    if is_help && fun.args.len() > 5 {
        return Err(Error::new(
            fun.args.last().unwrap().span(),
            "function's arity exceeds more than 5 arguments",
        ));
    } else if !is_help && fun.args.len() > 3 {
        return Err(Error::new(
            fun.args.last().unwrap().span(),
            "function's arity exceeds more than 3 arguments",
        ));
    }

    let context: Type = parse_quote!(&mut Context);
    let message: Type = parse_quote!(&Message);
    let args: Type = parse_quote!(Args);
    let options: Type = parse_quote!(&'static HelpOptions);
    let groups: Type = parse_quote!(&[&'static CommandGroup]);

    let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
    let context_path: Type = parse_quote!(&mut #crate_name::prelude::Context);
    let message_path: Type = parse_quote!(&#crate_name::model::channel::Message);
    let args_path: Type = parse_quote!(#crate_name::framework::standard::Args);
    let options_path: Type = parse_quote!(&'static #crate_name::framework::standard::HelpOptions);
    let groups_path: Type =
        parse_quote!(&[&'static #crate_name::framework::standard::CommandGroup]);

    let ctx_error = "first argument's type should be `&mut Context`";
    let msg_error = "second argument's type should be `&Message`";
    let args_error = "third argument's type should be `Args`";
    let options_error = "fourth argument's type should be `&'static HelpOptions`";
    let groups_error = "fifth argument's type should be `&[&'static CommandGroup]`";

    match fun.args.len() {
        0 => {
            fun.args.push(Argument {
                mutable: None,
                name: Ident::new("_ctx", Span::call_site()),
                kind: context_path,
            });
            fun.args.push(Argument {
                mutable: None,
                name: Ident::new("_msg", Span::call_site()),
                kind: message_path,
            });
            fun.args.push(Argument {
                mutable: Some(parse_quote!(mut)),
                name: Ident::new("_args", Span::call_site()),
                kind: args_path,
            });

            if is_help {
                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_options", Span::call_site()),
                    kind: options_path,
                });

                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_groups", Span::call_site()),
                    kind: groups_path,
                });
            }
        }
        1 => {
            if fun.args[0].kind != context {
                return Err(Error::new(fun.args[0].span(), ctx_error));
            }

            fun.args.push(Argument {
                mutable: None,
                name: Ident::new("_msg", Span::call_site()),
                kind: message_path,
            });
            fun.args.push(Argument {
                mutable: Some(parse_quote!(mut)),
                name: Ident::new("_args", Span::call_site()),
                kind: args_path,
            });

            if is_help {
                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_options", Span::call_site()),
                    kind: options_path,
                });

                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_groups", Span::call_site()),
                    kind: groups_path,
                });
            }
        }
        2 => {
            if fun.args[0].kind != context {
                return Err(Error::new(fun.args[0].span(), ctx_error));
            }

            if fun.args[1].kind != message {
                return Err(Error::new(fun.args[1].span(), msg_error));
            }

            fun.args.push(Argument {
                mutable: Some(parse_quote!(mut)),
                name: Ident::new("_args", Span::call_site()),
                kind: args_path,
            });

            if is_help {
                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_options", Span::call_site()),
                    kind: options_path,
                });

                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_groups", Span::call_site()),
                    kind: groups_path,
                });
            }
        }
        3 => {
            if fun.args[0].kind != context {
                return Err(Error::new(fun.args[0].span(), ctx_error));
            }

            if fun.args[1].kind != message {
                return Err(Error::new(fun.args[1].span(), msg_error));
            }

            if fun.args[2].kind != args {
                return Err(Error::new(fun.args[2].span(), args_error));
            }

            if is_help {
                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_options", Span::call_site()),
                    kind: options_path,
                });

                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_groups", Span::call_site()),
                    kind: groups_path,
                });
            }
        }
        4 => {
            if fun.args[0].kind != context {
                return Err(Error::new(fun.args[0].span(), ctx_error));
            }

            if fun.args[1].kind != message {
                return Err(Error::new(fun.args[1].span(), msg_error));
            }

            if fun.args[2].kind != args {
                return Err(Error::new(fun.args[2].span(), args_error));
            }

            if fun.args[3].kind != options {
                return Err(Error::new(fun.args[3].span(), options_error));
            }

            if is_help {
                fun.args.push(Argument {
                    mutable: None,
                    name: Ident::new("_groups", Span::call_site()),
                    kind: groups_path,
                });
            }
        }
        5 => {
            if fun.args[0].kind != context {
                return Err(Error::new(fun.args[0].span(), ctx_error));
            }

            if fun.args[1].kind != message {
                return Err(Error::new(fun.args[1].span(), msg_error));
            }

            if fun.args[2].kind != args {
                return Err(Error::new(fun.args[2].span(), args_error));
            }

            if fun.args[3].kind != options {
                return Err(Error::new(fun.args[3].span(), options_error));
            }

            if fun.args[4].kind != groups {
                return Err(Error::new(fun.args[4].span(), groups_error));
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}

fn validate_return_type(fun: &mut CommandFun) -> Result<()> {
    let span = fun.ret.span();
    let kind = match fun.ret {
        ReturnType::Type(_, ref kind) => kind,
        _ => unreachable!(),
    };

    let want: Type = parse_quote!(CommandResult);

    if &**kind != &want {
        return Err(Error::new(
            span,
            &format!(
                "expected a result as a return type, but got `{}`",
                quote!(#kind)
            ),
        ));
    }

    Ok(())
}

#[proc_macro_attribute]
pub fn command(attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut fun = parse_macro_input!(input as CommandFun);

    let _name = if !attr.is_empty() {
        parse_macro_input!(attr as Lit).to_str()
    } else {
        fun.name.to_string()
    };

    let mut options = Options::default();

    options.help_available = true;
    options.owner_privilege = true;

    for attribute in &fun.attributes {
        let span = attribute.span();
        let values = match parse_values(attribute) {
            Ok(vals) => vals,
            Err(err) => return err.to_compile_error().into(),
        };

        let name = values.name.to_string();
        match &name[..] {
            "checks" => {
                let Checks(idents) = Checks::parse(values);

                options.checks = idents;
            }
            "aliases" => {
                let Aliases(args) = Aliases::parse(values);

                options.names = args;
            }
            "description" => {
                let Description(arg) = Description::parse(values);

                options.desc = Some(arg);
            }
            "usage" => {
                let Usage(arg) = Usage::parse(values);

                options.usage = Some(arg);
            }
            "min_args" => {
                let MinArgs(arg) = MinArgs::parse(values);

                options.min_args = Some(arg);
            }
            "max_args" => {
                let MaxArgs(arg) = MaxArgs::parse(values);

                options.max_args = Some(arg);
            }
            "num_args" => {
                let NumArgs(arg) = NumArgs::parse(values);

                options.min_args = Some(arg);
                options.max_args = Some(arg);
            }
            "allowed_roles" => {
                let AllowedRoles(args) = AllowedRoles::parse(values);

                options.allowed_roles = args;
            }
            "help_available" => {
                let HelpAvailable(b) = HelpAvailable::parse(values);

                options.help_available = b;
            }
            "only_in" => {
                let Only(o) = Only::parse(values);

                options.only_in = o;
            }
            "owners_only" => {
                let OwnersOnly(b) = OwnersOnly::parse(values);

                options.owners_only = b;
            }
            "owner_privilege" => {
                let OwnerPrivilege(b) = OwnerPrivilege::parse(values);

                options.owner_privilege = b;
            }
            "sub" => {
                let SubCommands(s) = SubCommands::parse(values);

                options.sub = s;
            }
            "required_permissions" => {
                let RequiredPermissions(p) = RequiredPermissions::parse(values);

                let mut permissions = Permissions::default();
                for perm in p {
                    let p = match Permissions::from_str(&perm.to_string()) {
                        Some(p) => p,
                        None => return Error::new(perm.span(), "invalid permission").to_compile_error().into(),
                    };

                    // Add them together.
                    permissions.0 |= p.0;
                }

                options.required_permissions = permissions;
            }
            _ => {
                return Error::new(span, &format!("invalid attribute: {:?}", name))
                    .to_compile_error()
                    .into();
            }
        }
    }

    let Options {
        checks,
        names: aliases,
        desc,
        usage,
        min_args,
        max_args,
        allowed_roles,
        required_permissions,
        help_available,
        only_in,
        owners_only,
        owner_privilege,
        sub,
    } = options;

    let desc = AsOption(desc);
    let usage = AsOption(usage);
    let min_args = AsOption(min_args);
    let max_args = AsOption(max_args);

    if let Err(err) = validate_declaration(&mut fun, false) {
        return err.to_compile_error().into();
    }

    if let Err(err) = validate_return_type(&mut fun) {
        return err.to_compile_error().into();
    }

    let name = _name.clone();

    // If name starts with numbers, prepend an underscore to make it a valid identifier.
    let _name = if _name.starts_with(&['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] as &[char])
    {
        Ident::new(&format!("_{}", _name), Span::call_site())
    } else {
        Ident::new(&_name, Span::call_site())
    };

    let required_permissions = required_permissions.0;

    let options = _name.with_suffix(COMMAND_OPTIONS);
    let sub = sub
        .into_iter()
        .map(|i| i.with_suffix(COMMAND))
        .collect::<Vec<_>>();

    let n = _name.with_suffix(COMMAND);
    let nn = fun.name.clone();

    let cfgs = fun.cfgs.clone();
    let cfgs2 = cfgs.clone();

    let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
    let options_path = quote!(#crate_name::framework::standard::CommandOptions);
    let command_path = quote!(#crate_name::framework::standard::Command);
    let permissions_path = quote!(#crate_name::model::permissions::Permissions);

    (quote! {
        #(#cfgs)*
        pub static #options: #options_path = #options_path {
            checks: &[#(Check(#checks)),*],
            names: &[#name, #(#aliases),*],
            desc: #desc,
            usage: #usage,
            min_args: #min_args,
            max_args: #max_args,
            allowed_roles: &[#(#allowed_roles),*],
            required_permissions: #permissions_path { bits: #required_permissions },
            help_available: #help_available,
            only_in: #only_in,
            owners_only: #owners_only,
            owner_privilege: #owner_privilege,
            sub: &[#(&#sub),*],
        };

        #(#cfgs2)*
        pub static #n: #command_path = #command_path {
            fun: #nn,
            options: &#options,
        };

        #fun
    })
    .into()
}

#[proc_macro_attribute]
pub fn help(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut fun = parse_macro_input!(input as CommandFun);

    let mut options = HelpOptions::default();

    for attribute in &fun.attributes {
        let span = attribute.span();
        let values = match parse_values(attribute) {
            Ok(vals) => vals,
            Err(err) => return err.to_compile_error().into(),
        };

        let name = values.name.to_string();
        match &name[..] {
            "suggestion_text" => {
                let SuggestionText(s) = SuggestionText::parse(values);

                options.suggestion_text = s;
            }
            "no_help_available_text" => {
                let NoHelpAvailableText(s) = NoHelpAvailableText::parse(values);

                options.no_help_available_text = s;
            }
            "usage_label" => {
                let UsageLabel(s) = UsageLabel::parse(values);

                options.usage_label = s;
            }
            "usage_sample_label" => {
                let UsageSampleLabel(s) = UsageSampleLabel::parse(values);

                options.usage_sample_label = s;
            }
            "ungrouped_label" => {
                let UngroupedLabel(s) = UngroupedLabel::parse(values);

                options.ungrouped_label = s;
            }
            "grouped_label" => {
                let GroupedLabel(s) = GroupedLabel::parse(values);

                options.grouped_label = s;
            }
            "aliases_label" => {
                let AliasesLabel(s) = AliasesLabel::parse(values);

                options.aliases_label = s;
            }
            "description_label" => {
                let DescriptionLabel(s) = DescriptionLabel::parse(values);

                options.description_label = s;
            }
            "guild_only_text" => {
                let GuildOnlyText(s) = GuildOnlyText::parse(values);

                options.guild_only_text = s;
            }
            "dm_only_text" => {
                let DmOnlyText(s) = DmOnlyText::parse(values);

                options.dm_only_text = s;
            }
            "dm_and_guild_text" => {
                let DmAndGuildText(s) = DmAndGuildText::parse(values);

                options.dm_and_guild_text = s;
            }
            "available_text" => {
                let AvailableText(s) = AvailableText::parse(values);

                options.available_text = s;
            }
            "command_not_found_text" => {
                let CommandNotFoundText(s) = CommandNotFoundText::parse(values);

                options.command_not_found_text = s;
            }
            "individual_command_tip" => {
                let IndividualCommandTip(s) = IndividualCommandTip::parse(values);

                options.individual_command_tip = s;
            }
            "group_prefix" => {
                let GroupPrefix(s) = GroupPrefix::parse(values);

                options.group_prefix = s;
            }

            "striked_commands_tip_in_dm" => {
                let StrikedCommandsTipInDm(s) = StrikedCommandsTipInDm::parse(values);

                options.striked_commands_tip_in_dm = s;
            }
            "striked_commands_tip_in_guild" => {
                let StrikedCommandsTipInGuild(s) = StrikedCommandsTipInGuild::parse(values);

                options.striked_commands_tip_in_guild = s;
            }
            "lacking_role" => {
                let LackingRole(s) = LackingRole::parse(values);

                options.lacking_role = match HelpBehaviour::from_str(&s) {
                    Some(h) => h,
                    None => {
                        return Error::new(span, &format!("invalid help behaviour: {:?}", s))
                            .to_compile_error()
                            .into();
                    }
                };
            }
            "lacking_permissions" => {
                let LackingPermissions(s) = LackingPermissions::parse(values);

                options.lacking_permissions = match HelpBehaviour::from_str(&s) {
                    Some(h) => h,
                    None => {
                        return Error::new(span, &format!("invalid help behaviour: {:?}", s))
                            .to_compile_error()
                            .into();
                    }
                };
            }
            "wrong_channel" => {
                let WrongChannel(s) = WrongChannel::parse(values);

                options.wrong_channel = match HelpBehaviour::from_str(&s) {
                    Some(h) => h,
                    None => {
                        return Error::new(span, &format!("invalid help behaviour: {:?}", s))
                            .to_compile_error()
                            .into();
                    }
                };
            }
            "embed_error_colour" => {
                let EmbedErrorColour(s) = EmbedErrorColour::parse(values);

                options.embed_error_colour = s;
            }
            "embed_success_colour" => {
                let EmbedSuccessColour(s) = EmbedSuccessColour::parse(values);

                options.embed_success_colour = s;
            }
            "max_levenshtein_distance" => {
                let MaxLevenshteinDistance(s) = MaxLevenshteinDistance::parse(values);

                options.max_levenshtein_distance = s;
            }
            _ => {
                return Error::new(span, &format!("invalid attribute: {:?}", name))
                    .to_compile_error()
                    .into();
            }
        }
    }

    fn produce_strike_text(options: &HelpOptions, dm_or_guild: &str) -> Option<String> {
        use std::fmt::Write;

        let mut strike_text =
            String::from("~~`Strikethrough commands`~~ are unavailable because they");
        let mut is_any_option_strike = false;

        let mut concat_with_comma = if options.lacking_permissions == HelpBehaviour::Strike {
            is_any_option_strike = true;
            strike_text.push_str(" require permissions");

            true
        } else {
            false
        };

        if options.lacking_role == HelpBehaviour::Strike {
            is_any_option_strike = true;

            if concat_with_comma {
                strike_text.push_str(", require a specific role");
            } else {
                strike_text.push_str(" require a specific role");
                concat_with_comma = true;
            }
        }

        if options.wrong_channel == HelpBehaviour::Strike {
            is_any_option_strike = true;

            if concat_with_comma {
                let _ = write!(strike_text, ", or are limited to {}", dm_or_guild);
            } else {
                let _ = write!(strike_text, " are limited to {}", dm_or_guild);
            }
        }

        strike_text.push('.');

        if is_any_option_strike {
            Some(strike_text)
        } else {
            None
        }
    }

    if options.striked_commands_tip_in_dm == Some(String::new()) {
        options.striked_commands_tip_in_dm = produce_strike_text(&options, "direct messages");
    }

    if options.striked_commands_tip_in_guild == Some(String::new()) {
        options.striked_commands_tip_in_guild = produce_strike_text(&options, "guild messages");
    }

    let HelpOptions {
        suggestion_text,
        no_help_available_text,
        usage_label,
        usage_sample_label,
        ungrouped_label,
        grouped_label,
        aliases_label,
        description_label,
        guild_only_text,
        dm_only_text,
        dm_and_guild_text,
        available_text,
        command_not_found_text,
        individual_command_tip,
        group_prefix,
        striked_commands_tip_in_dm,
        striked_commands_tip_in_guild,
        lacking_role,
        lacking_permissions,
        wrong_channel,
        embed_error_colour,
        embed_success_colour,
        max_levenshtein_distance,
    } = options;

    let striked_commands_tip_in_dm = AsOption(striked_commands_tip_in_dm);
    let striked_commands_tip_in_guild = AsOption(striked_commands_tip_in_guild);

    if let Err(err) = validate_declaration(&mut fun, true) {
        return err.to_compile_error().into();
    }

    if let Err(err) = validate_return_type(&mut fun) {
        return err.to_compile_error().into();
    }

    let options = fun.name.with_suffix(HELP_OPTIONS);

    let n = fun.name.with_suffix(HELP);
    let nn = fun.name.clone();
    let cfgs = fun.cfgs.clone();
    let cfgs2 = cfgs.clone();

    let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
    let options_path = quote!(#crate_name::framework::standard::HelpOptions);
    let command_path = quote!(#crate_name::framework::standard::HelpCommand);

    (quote! {
        #(#cfgs)*
        pub static #options: #options_path = #options_path {
            suggestion_text: #suggestion_text,
            no_help_available_text: #no_help_available_text,
            usage_label: #usage_label,
            usage_sample_label: #usage_sample_label,
            ungrouped_label: #ungrouped_label,
            grouped_label: #grouped_label,
            aliases_label: #aliases_label,
            description_label: #description_label,
            guild_only_text: #guild_only_text,
            dm_only_text: #dm_only_text,
            dm_and_guild_text: #dm_and_guild_text,
            available_text: #available_text,
            command_not_found_text: #command_not_found_text,
            individual_command_tip: #individual_command_tip,
            group_prefix: #group_prefix,
            striked_commands_tip_in_dm: #striked_commands_tip_in_dm,
            striked_commands_tip_in_guild: #striked_commands_tip_in_guild,
            lacking_role: #lacking_role,
            lacking_permissions: #lacking_permissions,
            wrong_channel: #wrong_channel,
            embed_error_colour: #embed_error_colour,
            embed_success_colour: #embed_success_colour,
            max_levenshtein_distance: #max_levenshtein_distance,
        };

        #(#cfgs2)*
        pub static #n: #command_path = #command_path {
            fun: #nn,
            options: &#options,
        };

        #fun
    })
    .into()
}

#[proc_macro]
pub fn group(input: TokenStream) -> TokenStream {
    let group = parse_macro_input!(input as Group);

    group.into_token_stream().into()
}

#[proc_macro]
pub fn group_options(input: TokenStream) -> TokenStream {
    struct GroupOptionsName {
        name: Ident,
        options: GroupOptions,
    }

    impl Parse for GroupOptionsName {
        fn parse(input: ParseStream) -> Result<Self> {
            let name = input.parse::<Ident>()?;

            let options = input.parse::<GroupOptions>()?;

            Ok(GroupOptionsName { name, options })
        }
    }

    let GroupOptionsName { name, options } = parse_macro_input!(input as GroupOptionsName);

    let name = name.with_suffix(GROUP_OPTIONS);

    let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
    let options_path = quote!(#crate_name::framework::standard::GroupOptions);

    (quote! {
        pub static #name: #options_path = #options;
    })
    .into()
}
