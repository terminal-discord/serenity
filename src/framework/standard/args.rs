use uwl::StringStream;

use std::cell::Cell;
use std::error::Error as StdError;
use std::marker::PhantomData;
use std::{fmt, str::FromStr};

/// Defines how an operation on an `Args` method failed.
#[derive(Debug)]
pub enum Error<E> {
    /// "END-OF-STRING". There's nothing to parse anymore.
    Eos,
    /// Parsing operation failed.
    Parse(E),
}

impl<E> From<E> for Error<E> {
    fn from(e: E) -> Self {
        Error::Parse(e)
    }
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match *self {
            Eos => write!(f, "ArgError(\"end of string\")"),
            Parse(ref e) => write!(f, "ArgError(\"{}\")", e),
        }
    }
}

impl<E: fmt::Debug + fmt::Display> StdError for Error<E> {
    fn description(&self) -> &str {
        match self {
            Error::Eos => "end-of-string",
            Error::Parse(_) => "parse-failure",
        }
    }
}

type Result<T, E> = ::std::result::Result<T, Error<E>>;

#[derive(Debug)]
pub enum Delimiter {
    Single(char),
    Multiple(String),
}

impl From<char> for Delimiter {
    #[inline]
    fn from(c: char) -> Delimiter {
        Delimiter::Single(c)
    }
}

impl From<String> for Delimiter {
    #[inline]
    fn from(s: String) -> Delimiter {
        Delimiter::Multiple(s)
    }
}

impl<'a> From<&'a String> for Delimiter {
    #[inline]
    fn from(s: &'a String) -> Delimiter {
        Delimiter::Multiple(s.clone())
    }
}

impl<'a> From<&'a str> for Delimiter {
    #[inline]
    fn from(s: &'a str) -> Delimiter {
        Delimiter::Multiple(s.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum TokenKind {
    Delimiter,
    Argument,
    QuotedArgument,
}

#[derive(Debug, Clone, Copy)]
struct Token {
    kind: TokenKind,
    start: usize,
    end: usize,
}

impl Token {
    #[inline]
    fn new(kind: TokenKind, start: usize, end: usize) -> Self {
        Token { kind, start, end }
    }
}

fn lex(stream: &mut StringStream<'_>, delims: &[&Delimiter]) -> Option<Token> {
    if stream.at_end() {
        return None;
    }

    for delim in delims {
        match delim {
            Delimiter::Single(c) => {
                if stream.current()?.contains(*c) {
                    let start = stream.offset();
                    stream.next();
                    return Some(Token::new(TokenKind::Delimiter, start, stream.offset()));
                }
            }
            Delimiter::Multiple(s) => {
                if *s == stream.peek_for(s.chars().count()) {
                    let start = stream.offset();
                    for _ in 0..s.len() {
                        stream.next();
                    }

                    return Some(Token::new(TokenKind::Delimiter, start, stream.offset()));
                }
            }
        }
    }

    if stream.current()? == "\"" {
        let start = stream.offset();
        stream.next();

        stream.take_until(|s| s == "\"");

        let is_quote = stream.current().map_or(false, |s| s == "\"");
        stream.next();

        let end = stream.offset();

        return Some(if is_quote {
            Token::new(TokenKind::QuotedArgument, start, end)
        } else {
            // We're missing an end quote. View this as a normal argument.
            Token::new(TokenKind::Argument, start, stream.src.len())
        });
    }

    let start = stream.offset();

    'outer: while !stream.at_end() {
        for delim in delims {
            match delim {
                Delimiter::Single(c) => {
                    if stream.current()?.contains(*c) {
                        break 'outer;
                    }
                }
                Delimiter::Multiple(s) => {
                    if *s == stream.peek_for(s.chars().count()) {
                        break 'outer;
                    }
                }
            }
        }

        stream.next();
    }

    Some(Token::new(TokenKind::Argument, start, stream.offset()))
}

#[derive(Debug, Clone, Copy)]
enum State {
    None,
    Quoted,
    Trimmed,
    // Preserve the order they were called.
    QuotedTrimmed,
    TrimmedQuoted,
}

#[derive(Clone, Debug)]
pub struct Args {
    message: String,
    args: Vec<Token>,
    offset: usize,
    state: Cell<State>,
}

impl Args {
    pub fn new(message: &str, possible_delimiters: &[Delimiter]) -> Self {
        let delims = possible_delimiters
            .iter()
            .filter(|d| match d {
                Delimiter::Single(c) => message.contains(*c),
                Delimiter::Multiple(s) => message.contains(s),
            })
            .collect::<Vec<_>>();

        let mut args = Vec::new();

        // If there are no delimiters, then the only possible argument is the whole message.
        if delims.is_empty() && !message.is_empty() {
            args.push(Token::new(TokenKind::Argument, 0, message.len()));
        } else {
            let mut stream = StringStream::new(message);

            while let Some(token) = lex(&mut stream, &delims) {
                if token.kind == TokenKind::Delimiter {
                    continue;
                }

                args.push(token);
            }
        }

        Args {
            args,
            message: message.to_string(),
            offset: 0,
            state: Cell::new(State::None),
        }
    }

    fn span(&self) -> (usize, usize) {
        let Token {
            kind: _,
            start,
            end,
        } = &self.args[self.offset];

        let start = *start;
        let end = *end;

        (start, end)
    }

    #[inline]
    fn slice(&self) -> &str {
        let (start, end) = self.span();

        &self.message[start..end]
    }

    /// Move to the next argument.
    /// This increments the offset pointer.
    pub fn next(&mut self) -> &mut Self {
        if self.is_empty() {
            return self;
        }

        self.offset += 1;

        self
    }

    /// Go one step behind.
    /// This decrements the offset pointer.
    #[inline]
    pub fn rewind(&mut self) -> &mut Self {
        if self.offset == 0 {
            return self;
        }

        self.offset -= 1;

        self
    }

    /// Go back to the starting point.
    #[inline]
    pub fn restore(&mut self) {
        self.offset = 0;
    }

    fn apply<'a>(&self, s: &'a str) -> &'a str {
        fn remove_quotes(s: &str) -> &str {
            if s.starts_with('"') && s.ends_with('"') {
                return &s[1..s.len() - 1];
            }

            s
        }

        fn trim(s: &str) -> &str {
            let trimmed = s.trim();

            // Search where the argument starts and ends between the whitespace.
            let start = s.find(trimmed).unwrap_or(0);
            let end = start + trimmed.len();

            &s[start..end]
        }

        let mut s = s;

        match self.state.get() {
            State::None => {}
            State::Quoted => {
                s = remove_quotes(s);
            }
            State::Trimmed => {
                s = trim(s);
            }
            State::QuotedTrimmed => {
                s = remove_quotes(s);
                s = trim(s);
            }
            State::TrimmedQuoted => {
                s = trim(s);
                s = remove_quotes(s);
            }
        }

        self.state.set(State::None);

        s
    }

    /// Retrieve the current argument.
    #[inline]
    pub fn current(&self) -> Option<&str> {
        if self.is_empty() {
            return None;
        }

        let mut s = self.slice();
        s = self.apply(s);

        Some(s)
    }

    /// Apply trimming the next time the current argument is accessed.
    pub fn trimmed(&mut self) -> &mut Self {
        if self.is_empty() {
            return self;
        }

        match self.state.get() {
            State::None => self.state.set(State::Trimmed),
            State::Quoted => self.state.set(State::QuotedTrimmed),
            _ => {}
        }

        self
    }

    /// Remove quotations surrounding the current argument the next time it is accessed.
    pub fn quoted(&mut self) -> &mut Self {
        if self.is_empty() {
            return self;
        }

        let is_quoted = self.args[self.offset].kind == TokenKind::QuotedArgument;

        if is_quoted {
            match self.state.get() {
                State::None => self.state.set(State::Quoted),
                State::Trimmed => self.state.set(State::TrimmedQuoted),
                _ => {}
            }
        }

        return self;
    }

    /// Parse the current argument.
    #[inline]
    pub fn parse<T: FromStr>(&self) -> Result<T, T::Err> {
        T::from_str(self.current().ok_or(Error::Eos)?).map_err(Error::Parse)
    }

    /// Parse the current argument and advance.
    #[inline]
    pub fn single<T: FromStr>(&mut self) -> Result<T, T::Err> {
        let p = self.parse::<T>()?;
        self.next();
        Ok(p)
    }

    /// By starting from the current offset, iterate over
    /// any available arguments until there are none.
    ///
    /// # Examples
    ///
    /// Assert that all of the numbers in the message are even.
    ///
    /// ```rust
    /// use serenity::framework::standard::{Args, Delimiter};
    ///
    /// let mut args = Args::new("4 2", &[Delimiter::Single(' ')]);
    ///
    /// for arg in args.iter::<u32>() {
    ///     // Zero troubles, zero worries.
    ///     let arg = arg.unwrap_or(0);
    ///     assert!(arg % 2 == 0);
    /// }
    ///
    /// assert!(args.is_empty());
    /// ```
    #[inline]
    pub fn iter<T: FromStr>(&mut self) -> Iter<T> {
        Iter {
            args: self,
            state: State::None,
            _marker: PhantomData,
        }
    }

    /// Remove surrounding quotations, if present, from the argument; parse it and advance.
    #[inline]
    pub fn single_quoted<T: FromStr>(&mut self) -> Result<T, T::Err> {
        let p = self.quoted().parse::<T>()?;
        self.next();
        Ok(p)
    }

    /// Search for any available argument that can be parsed, and remove it from the "arguments queue".
    ///
    /// # Note
    /// The removal is irreversible. And happens after the search *and* the parse were successful.
    ///
    /// # Note 2
    /// "Arguments queue" is the list which contains all arguments that were deemed unique as defined by quotations and delimiters.
    /// The 'removed' argument can be, likewise, still accessed via `message`.
    pub fn find<T: FromStr>(&mut self) -> Result<T, T::Err> {
        if self.is_empty() {
            return Err(Error::Eos);
        }

        let before = self.offset;
        self.restore();

        let pos = match self.iter::<T>().quoted().position(|res| res.is_ok()) {
            Some(p) => p,
            None => return Err(Error::Eos),
        };

        self.offset = pos;
        let parsed = self.single_quoted::<T>()?;

        self.args.remove(pos);
        self.offset = before;
        self.rewind();

        Ok(parsed)
    }

    /// Search for any available argument that can be parsed.
    pub fn find_n<T: FromStr>(&mut self) -> Result<T, T::Err> {
        if self.is_empty() {
            return Err(Error::Eos);
        }

        let before = self.offset;
        self.restore();
        let pos = match self.iter::<T>().quoted().position(|res| res.is_ok()) {
            Some(p) => p,
            None => return Err(Error::Eos),
        };

        self.offset = pos;
        let parsed = self.quoted().parse::<T>()?;

        self.offset = before;

        Ok(parsed)
    }

    /// Get the original, unmodified message passed to the command.
    #[inline]
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Starting from the offset, return the remainder of available arguments.
    #[inline]
    pub fn rest(&self) -> &str {
        if self.is_empty() {
            return "";
        }

        let (start, _) = self.span();

        &self.message[start..]
    }

    /// Return the full amount of recognised arguments.
    /// The length of the "arguments queue".
    ///
    /// # Note
    ///
    /// The value returned is to be assumed to stay static.
    /// However, if `find` was called previously, and was succesful, then the value is substracted by one.
    #[inline]
    pub fn len(&self) -> usize {
        self.args.len()
    }

    /// Assert that there are no more arguments left.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.offset >= self.len()
    }

    /// Return the amount of arguments still available.
    #[inline]
    pub fn remaining(&self) -> usize {
        if self.is_empty() {
            return 0;
        }

        self.len() - self.offset
    }
}

/// Parse each argument individually, as an iterator.
pub struct Iter<'a, T: FromStr> {
    args: &'a mut Args,
    state: State,
    _marker: PhantomData<T>,
}

impl<'a, T: FromStr> Iter<'a, T> {
    #[inline]
    pub fn quoted(&mut self) -> &mut Self {
        match self.state {
            State::None => self.state = State::Quoted,
            State::Trimmed => self.state = State::TrimmedQuoted,
            _ => {}
        }

        self
    }

    #[inline]
    pub fn trimmed(&mut self) -> &mut Self {
        match self.state {
            State::None => self.state = State::Trimmed,
            State::Quoted => self.state = State::QuotedTrimmed,
            _ => {}
        }

        self
    }
}

impl<'a, T: FromStr> Iterator for Iter<'a, T> {
    type Item = Result<T, T::Err>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.args.is_empty() {
            None
        } else {
            match self.state {
                State::Quoted => {
                    self.args.quoted();
                }
                State::Trimmed => {
                    self.args.trimmed();
                }
                State::QuotedTrimmed => {
                    self.args.quoted().trimmed();
                }
                State::TrimmedQuoted => {
                    self.args.trimmed().quoted();
                }
                State::None => {}
            }

            let arg = self.args.parse::<T>();
            self.args.next();
            Some(arg)
        }
    }
}
