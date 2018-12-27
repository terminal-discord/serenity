// It has become too of a burden for the `quote!` macro.
#![recursion_limit = "128"]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use std::cell::RefCell;

use self::proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;

use quote::ToTokens;

use syn::parse::{Error, Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{braced, bracketed, parenthesized};
use syn::{parse_macro_input, parse_quote, Token};
use syn::{ArgCaptured, Pat, Type};
use syn::{Attribute, Block, FnArg, Ident, Lit, ReturnType};

use syn::ext::IdentExt;
use syn::Stmt;

mod attributes;
mod util;

use self::attributes::*;
use self::util::*;

thread_local! {
    static CRATE_NAME: RefCell<String> = RefCell::new(String::new());
}

#[proc_macro]
pub fn initialize(input: TokenStream) -> TokenStream {
    let name = parse_macro_input!(input as Ident);
    let mut name = name.to_string();

    if name == "_crate" {
        // Discard the underscore.
        name.remove(0);
    }

    CRATE_NAME.with(|cn| cn.replace(name.to_string()));

    TokenStream::new()
}

#[derive(Debug, PartialEq)]
enum OnlyIn {
    Dm,
    Guild,
    None,
}

impl ToTokens for OnlyIn {
    fn to_tokens(&self, stream: &mut TokenStream2) {
        let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
        let only_in_path = quote!(#crate_name::framework::standard::OnlyIn);
        match self {
            OnlyIn::Dm => stream.extend(quote!(#only_in_path::Dm)),
            OnlyIn::Guild => stream.extend(quote!(#only_in_path::Guild)),
            OnlyIn::None => stream.extend(quote!(#only_in_path::None)),
        }
    }
}

impl Default for OnlyIn {
    #[inline]
    fn default() -> Self {
        OnlyIn::None
    }
}

#[derive(Debug)]
struct CommandFun {
    cfgs: Vec<Attribute>,
    attributes: Vec<Attribute>,
    name: Ident,
    args: Vec<Argument>,
    ret: ReturnType,
    body: Vec<Stmt>,
}

impl Parse for CommandFun {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attributes = input.call(Attribute::parse_outer)?;

        // Omit doc comments.
        attributes.retain(|a| !a.path.is_ident("doc"));

        let (cfgs, attributes): (Vec<_>, Vec<_>) = attributes.into_iter().partition(|a| a.path.is_ident("cfg"));

        if input.peek(Token![pub]) {
            input.parse::<Token![pub]>()?;
        }

        input.parse::<Token![fn]>()?;
        let name = input.parse()?;
        // (....)
        let pcont;
        parenthesized!(pcont in input);
        let args: Punctuated<FnArg, Token![,]> = pcont.parse_terminated(FnArg::parse)?;

        let ret = if input.peek(Token![->]) {
            input.parse()?
        } else {
            return Err(Error::new(input.cursor().span(), "expected a return type"));
        };

        // { ... }
        let bcont;
        braced!(bcont in input);
        let body = bcont.call(Block::parse_within)?;

        let args: ::std::result::Result<Vec<Argument>, _> = args
            .into_iter()
            .map(|arg| {
                let span = arg.span();
                match arg {
                    FnArg::Captured(ArgCaptured {
                        pat,
                        colon_token: _,
                        ty: kind,
                    }) => {
                        let span = pat.span();
                        match pat {
                            Pat::Ident(id) => {
                                let name = id.ident;
                                let mutable = id.mutability;

                                Ok(Argument {
                                    mutable,
                                    name,
                                    kind,
                                })
                            }
                            Pat::Wild(wild) => {
                                let token = wild.underscore_token;

                                let name = Ident::new("_", token.spans[0]);

                                Ok(Argument {
                                    mutable: None,
                                    name,
                                    kind,
                                })
                            }
                            _ => Err(Error::new(span, &format!("unsupported pattern: {:?}", pat))),
                        }
                    }
                    _ => Err(Error::new(
                        span,
                        &format!("use of a prohibited argument type: {:?}", arg),
                    )),
                }
            })
            .collect();

        let args = args?;

        Ok(CommandFun {
            cfgs,
            attributes,
            name,
            args,
            ret,
            body,
        })
    }
}

impl ToTokens for CommandFun {
    fn to_tokens(&self, stream: &mut TokenStream2) {
        let CommandFun {
            cfgs,
            attributes: _,
            name,
            args,
            ret,
            body,
        } = self;

        stream.extend(quote! {
            #(#cfgs)*
            pub fn #name (#(#args),*) #ret {
                #(#body)*
            }
        });
    }
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

#[derive(Debug, Default)]
struct Options {
    pub checks: Vec<Ident>,
    pub names: Vec<String>,
    pub desc: Option<String>,
    pub usage: Option<String>,
    pub min_args: Option<u8>,
    pub max_args: Option<u8>,
    pub allowed_roles: Vec<String>,
    pub help_available: bool,
    pub only_in: OnlyIn,
    pub owners_only: bool,
    pub owner_privilege: bool,
    pub sub: Vec<Ident>,
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
        let values = match parse_values(attribute.clone()) {
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

    let options = _name.to_command_options();
    let sub = sub.into_iter().map(|i| i.to_command()).collect::<Vec<_>>();

    let n = _name.to_command();
    let nn = fun.name.clone();

    let cfgs = fun.cfgs.clone();
    let cfgs2 = cfgs.clone();

    let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
    let options_path = quote!(#crate_name::framework::standard::CommandOptions);
    let command_path = quote!(#crate_name::framework::standard::Command);
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

#[derive(PartialEq, Debug)]
enum HelpBehaviour {
    Strike,
    Hide,
    Nothing,
}

impl HelpBehaviour {
    fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "strike" => HelpBehaviour::Strike,
            "hide" => HelpBehaviour::Hide,
            "nothing" => HelpBehaviour::Nothing,
            _ => return None,
        })
    }
}

impl ToTokens for HelpBehaviour {
    fn to_tokens(&self, stream: &mut TokenStream2) {
        let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
        let help_behaviour_path = quote!(#crate_name::framework::standard::HelpBehaviour);
        match self {
            HelpBehaviour::Strike => stream.extend(quote!(#help_behaviour_path::Strike)),
            HelpBehaviour::Hide => stream.extend(quote!(#help_behaviour_path::Hide)),
            HelpBehaviour::Nothing => stream.extend(quote!(#help_behaviour_path::Nothing)),
        }
    }
}

#[derive(Debug, PartialEq)]
struct HelpOptions {
    suggestion_text: String,
    no_help_available_text: String,
    usage_label: String,
    usage_sample_label: String,
    ungrouped_label: String,
    description_label: String,
    grouped_label: String,
    aliases_label: String,
    guild_only_text: String,
    dm_only_text: String,
    dm_and_guild_text: String,
    available_text: String,
    command_not_found_text: String,
    individual_command_tip: String,
    striked_commands_tip_in_dm: Option<String>,
    striked_commands_tip_in_guild: Option<String>,
    group_prefix: String,
    lacking_role: HelpBehaviour,
    lacking_permissions: HelpBehaviour,
    wrong_channel: HelpBehaviour,
    embed_error_colour: u64,
    embed_success_colour: u64,
    max_levenshtein_distance: usize,
}

impl Default for HelpOptions {
    fn default() -> HelpOptions {
        HelpOptions {
            suggestion_text: "Did you mean `{}`?".to_string(),
            no_help_available_text: "**Error**: No help available.".to_string(),
            usage_label: "Usage".to_string(),
            usage_sample_label: "Sample usage".to_string(),
            ungrouped_label: "Ungrouped".to_string(),
            grouped_label: "Group".to_string(),
            aliases_label: "Aliases".to_string(),
            description_label: "Description".to_string(),
            guild_only_text: "Only in guilds".to_string(),
            dm_only_text: "Only in DM".to_string(),
            dm_and_guild_text: "In DM and guilds".to_string(),
            available_text: "Available".to_string(),
            command_not_found_text: "**Error**: Command `{}` not found.".to_string(),
            individual_command_tip: "To get help with an individual command, pass its \
                                     name as an argument to this command."
                .to_string(),
            group_prefix: "Prefix".to_string(),
            striked_commands_tip_in_dm: Some(String::new()),
            striked_commands_tip_in_guild: Some(String::new()),
            lacking_role: HelpBehaviour::Strike,
            lacking_permissions: HelpBehaviour::Strike,
            wrong_channel: HelpBehaviour::Strike,
            embed_error_colour: 0x992D22,   // DARK_RED
            embed_success_colour: 0xF6DBD8, // ROSEWATER
            max_levenshtein_distance: 0,
        }
    }
}

#[proc_macro_attribute]
pub fn help(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut fun = parse_macro_input!(input as CommandFun);

    let mut options = HelpOptions::default();

    for attribute in &fun.attributes {
        let span = attribute.span();
        let values = match parse_values(attribute.clone()) {
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

    let options = fun.name.to_help_options();

    let n = fun.name.to_help_command();
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

#[derive(Debug, Default)]
struct GroupOptions {
    prefixes: Vec<String>,
    only: OnlyIn,
    owner_only: bool,
    owner_privilege: bool,
    help_available: bool,
    allowed_roles: Vec<String>,
    checks: Vec<Ident>,
    default_command: Option<Ident>,
    description: Option<String>,
    inherit: Option<IdentAccess>,
}

impl Parse for GroupOptions {
    fn parse(input: ParseStream) -> Result<Self> {
        let Object(fields) = input.parse::<Object>()?;

        let mut options = GroupOptions::default();

        options.help_available = true;
        options.owner_privilege = true;

        for Field { name, value } in fields {
            let span = name.span();
            let name = name.to_string();
            match (&name[..], value) {
                ("prefixes", Expr::Array(Array(values)))
                | ("allowed_roles", Expr::Array(Array(values))) => {
                    let values = values
                        .into_iter()
                        .map(|l| match l {
                            Expr::Lit(l) => l.to_str(),
                            _ => panic!("expected a list of strings"),
                        })
                        .collect();

                    if name == "prefixes" {
                        options.prefixes = values;
                    } else {
                        options.allowed_roles = values;
                    }
                }
                ("only", Expr::Lit(value)) => {
                    let span = value.span();
                    let value = value.to_str();

                    let only = match &value[..] {
                        "dms" => OnlyIn::Dm,
                        "guilds" => OnlyIn::Guild,
                        _ => return Err(Error::new(span, "invalid only option")),
                    };

                    options.only = only;
                }
                ("owner_only", Expr::Lit(value))
                | ("owner_privilege", Expr::Lit(value))
                | ("help_available", Expr::Lit(value)) => {
                    let b = value.to_bool();

                    if name == "owner_only" {
                        options.owner_only = b;
                    } else if name == "owner_privilege" {
                        options.owner_privilege = b;
                    } else {
                        options.help_available = b;
                    }
                }
                ("checks", Expr::Array(Array(arr))) => {
                    let idents = arr
                        .into_iter()
                        .map(|l| match l {
                            Expr::Access(IdentAccess(l, None)) => l,
                            _ => panic!("invalid value, expected ident {:?}", l),
                        })
                        .collect();

                    options.checks = idents;
                }
                ("default_command", Expr::Access(IdentAccess(re, _))) => {
                    options.default_command = Some(re);
                }
                ("prefix", Expr::Lit(s)) | ("description", Expr::Lit(s)) => {
                    let s = s.to_str();

                    if name == "prefix" {
                        options.prefixes = vec![s];
                    } else {
                        options.description = Some(s);
                    }
                }
                ("inherit", Expr::Access(access)) => {
                    options.inherit = Some(access);
                }
                (name, _) => {
                    return Err(Error::new(
                        span,
                        &format!("`{}` is not a valid group option", name),
                    ));
                }
            }
        }
        Ok(options)
    }
}

impl ToTokens for GroupOptions {
    fn to_tokens(&self, stream: &mut TokenStream2) {
        let GroupOptions {
            prefixes,
            allowed_roles,
            owner_privilege,
            owner_only,
            help_available,
            only,
            description,
            checks,
            default_command,
            inherit,
        } = self;

        let description = AsOption(description.clone());
        let mut dc = quote! { None };

        if let Some(cmd) = default_command {
            let cmd = cmd.to_command();
            dc = quote! {
                Some(&#cmd)
            };
        }

        let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
        let options_path = quote!(#crate_name::framework::standard::GroupOptions);

        if let Some(IdentAccess(from, its)) = inherit {
            let inherit = match its {
                Some(its) => {
                    if its != "options" {
                        *stream = Error::new(its.span(), "field being accessed is not `options`")
                            .to_compile_error();
                        return;
                    }
                    let from = from.to_group();
                    quote! { *(#from).#its }
                }
                None => {
                    let from = from.to_group_options();
                    quote! { #from }
                }
            };

            let description = if description.0.is_some() {
                quote! { description: #description, }
            } else {
                quote!()
            };

            let prefixes = if !prefixes.is_empty() {
                quote! { prefixes: &[#(#prefixes),*], }
            } else {
                quote!()
            };

            let allowed_roles = if !allowed_roles.is_empty() {
                quote! { allowed_roles: &[#(#allowed_roles),*], }
            } else {
                quote!()
            };

            let owner_privilege = if !owner_privilege {
                quote! { owner_privilege: #owner_privilege, }
            } else {
                quote!()
            };

            let owner_only = if *owner_only {
                quote! { owner_only: #owner_only, }
            } else {
                quote!()
            };

            let help_available = if !help_available {
                quote! { help_available: #help_available, }
            } else {
                quote!()
            };

            let only = if *only != OnlyIn::None {
                quote! { only: #only, }
            } else {
                quote!()
            };

            let checks = if !checks.is_empty() {
                quote! { checks: &[#(Check(#checks)),*], }
            } else {
                quote!()
            };

            let default_command = if default_command.is_some() {
                quote! { default_command: #dc, }
            } else {
                quote!()
            };

            stream.extend(quote! {
                #options_path {
                    #prefixes
                    #allowed_roles
                    #owner_privilege
                    #owner_only
                    #help_available
                    #only
                    #description
                    #checks
                    #default_command
                    ..#inherit
                }
            });
        } else {
            stream.extend(quote! {
                #options_path {
                    prefixes: &[#(#prefixes),*],
                    allowed_roles: &[#(#allowed_roles),*],
                    owner_privilege: #owner_privilege,
                    owners_only: #owner_only,
                    help_available: #help_available,
                    only: #only,
                    description: #description,
                    checks: &[#(Check(#checks)),*],
                    default_command: #dc,
                }
            });
        }
    }
}

#[derive(Debug)]
struct Group {
    name: Ident,
    options: RefOrInstance<GroupOptions>,
    commands: Punctuated<Ident, Token![,]>,
    sub: Vec<RefOrInstance<Group>>,
}

impl Parse for Group {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        braced!(content in input);

        let input = content;

        let Field { name, value } = input.parse::<Field<Lit>>()?;
        if name != "name" {
            return Err(Error::new(name.span(), "first key needs to be `name`"));
        }

        let name = value.to_ident();

        input.parse::<Token![,]>()?;

        let Field {
            name: n,
            value: options,
        } = input.parse::<Field<RefOrInstance<GroupOptions>>>()?;
        if n != "options" {
            return Err(Error::new(n.span(), "second key needs to be `options`"));
        }

        input.parse::<Token![,]>()?;

        let commands = input.parse::<Ident>()?;
        if commands != "commands" {
            return Err(Error::new(
                commands.span(),
                "third key needs to be `commands`",
            ));
        }

        input.parse::<Token![:]>()?;

        let content;
        bracketed!(content in input);

        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
        }

        let mut sub = Vec::new();

        if let Ok(s) = input.parse::<Ident>() {
            if s != "sub" {
                return Err(Error::new(s.span(), "fourth key needs to be `sub`"));
            }

            input.parse::<Token![:]>()?;

            let content;
            bracketed!(content in input);

            let refs: Punctuated<_, Token![,]> = content.parse_terminated(RefOrInstance::parse)?;

            sub.extend(refs.into_iter());
        }

        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
        }

        Ok(Group {
            name,
            options,
            commands: content.parse_terminated(Ident::parse_any)?,
            sub,
        })
    }
}

impl ToTokens for Group {
    fn to_tokens(&self, stream: &mut TokenStream2) {
        let Group {
            name,
            options: opts,
            commands,
            sub,
        } = self;

        let commands = commands.into_iter().map(|cmd| {
            let cmd = cmd.to_command();
            quote! {
                &#cmd
            }
        });

        let sub = sub
            .into_iter()
            .map(|group| match group {
                RefOrInstance::Instance(group) => {
                    let name = group.name.to_group();
                    stream.extend(group.into_token_stream());

                    name
                }
                RefOrInstance::Ref(name) => name.to_group(),
            })
            .collect::<Vec<_>>();

        let mut group_ops = name.to_group_options();
        let n = name.to_string();
        let name = name.to_group();

        let mut options = None;
        match opts {
            RefOrInstance::Ref(name) => group_ops = name.to_group_options(),
            RefOrInstance::Instance(opt) => options = Some(opt),
        }

        let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
        let options_path = quote!(#crate_name::framework::standard::GroupOptions);
        let group_path = quote!(#crate_name::framework::standard::CommandGroup);

        if options.is_some() {
            stream.extend(quote! {
                pub static #group_ops: #options_path = #options;
            });
        }

        stream.extend(quote! {
            pub static #name: #group_path = #group_path {
                name: #n,
                options: &#group_ops,
                commands: &[#(#commands),*],
                sub: &[#(&#sub),*]
            };
        });
    }
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

    let name = name.to_group_options();

    let crate_name = CRATE_NAME.with(|cn| Ident::new(&cn.borrow(), Span::call_site()));
    let options_path = quote!(#crate_name::framework::standard::GroupOptions);

    (quote! {
        pub static #name: #options_path = #options;
    })
    .into()
}
