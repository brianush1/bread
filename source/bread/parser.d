module bread.parser;
import bread.source;
import bread.ast;
import sumtype;
import std.typecons;
import std.algorithm;
import std.range;
import std.utf;
import std.utf : stride, count;
import std.conv;

immutable(string[]) SYMBOLS = [
	"(", ")", "[", "]", "{", "}",
	"!", ".", ",", ":", ";",
	"..", "/", "=",
	"+", "-", "*", "**",
].sort!((a, b) => b.count < a.count).array;

struct SymbolId {
	ushort value;

	string toString() const {
		return SYMBOLS[value];
	}
}

static foreach (i, symbol; SYMBOLS) {
	template Symbol(string T) if (T == symbol) {
		enum Symbol = SymbolId(cast(ushort) i);
	}
}

immutable(string[]) KEYWORDS = [
	"nil",
	"type", "if", "else", "do", "return", "static",
	"template",
	"const", "immutable", "function", "struct",
	"new", "import", "__builtin_value__",
	"pragma",
];

struct KeywordId {
	ushort value;

	string toString() const {
		return KEYWORDS[value];
	}
}

static foreach (i, keyword; KEYWORDS) {
	template Keyword(string T) if (T == keyword) {
		enum Keyword = KeywordId(cast(ushort) i);
	}
}

struct Token {
	struct Eof {}

	struct Unknown {
		string value;
	}

	struct Identifier {
		string name;
	}

	struct Keyword {
		KeywordId id;
	}

	struct String {
		string value;
	}

	struct Char {
		dchar value;
	}

	struct Int {
		ulong value;
	}

	struct Float {
		float value;
	}

	struct Double {
		double value;
	}

	struct Symbol {
		SymbolId id;
	}

	struct Comment {
		string value;
	}

	Span span;
	alias Payload = SumType!(
		Eof,
		Unknown,
		Identifier,
		Keyword,
		String,
		Char,
		Int,
		Float,
		Double,
		Symbol,
		Comment,
	);
	Payload payload;
	alias payload this;

	string toString() const {
		return payload.match!(
			(Eof token) => "eof",
			(Unknown token) => "unknown '" ~ token.value.to!string ~ "'",
			(Identifier token) => "identifier '" ~ token.name.to!string ~ "'",
			(Keyword token) => "keyword '" ~ token.id.to!string ~ "'",
			(String token) => "string \"" ~ token.value.to!string ~ "\"", // TODO: escapes
			(Char token) => "char '" ~ token.value.to!string ~ "'",
			(Int token) => "int '" ~ token.value.to!string ~ "'",
			(Float token) => "float '" ~ token.value.to!string ~ "'",
			(Double token) => "double '" ~ token.value.to!string ~ "'",
			(Symbol token) => "symbol '" ~ token.id.to!string ~ "'",
			(Comment token) => "comment '" ~ token.value.to!string ~ "'",
		);
	}
}

private struct Chars {

	static bool isSpace(dchar c) {
		return c == ' ' || c == '\t' || c == '\u000B' || c == '\u000C'
			|| c == '\n' || c == '\r';
	}

	static bool isIdentifierStart(dchar c) {
		return c == '_' || (c >= 'a' && c <= 'z')
			|| (c >= 'A' && c <= 'Z');
	}

	static bool isIdentifier(dchar c) {
		return isIdentifierStart(c) || (c >= '0' && c <= '9');
	}

	static bool isDigit(dchar c) {
		return c >= '0' && c <= '9';
	}

}

class ParsingException : Exception {

	Diagnostic diagnostic;

	this(Args...)(Args args) {
		super("A parsing error has occurred");
		diagnostic = Diagnostic(args);
	}

}

final class Lexer {

	this(Source source) {
		this.source = source;
	}

private:

	this() {}

	Source source;

	Nullable!Token peekedToken;

	size_t index;

	dchar peekChar() {
		if (index >= source.length) {
			return 0;
		}
		return source[index .. $].front;
	}

	string peekChars(size_t length) {
		if (index >= source.length)
			return "";
		string s = source[index .. $];
		size_t at;
		foreach (i; 0 .. length) {
			if (at >= s.length)
				return "";
			at += s.stride(at);
		}
		return source[index .. index + at];
	}

	dchar nextChar() {
		if (index >= source.length) {
			return 0;
		}
		dchar result = peekChar;
		index += source.stride(index);
		return result;
	}

	string nextChars(size_t length) {
		if (index >= source.length)
			return "";
		string s = source[index .. $];
		size_t at;
		foreach (i; 0 .. length) {
			if (at >= s.length)
				return "";
			at += s.stride(at);
		}
		index += at;
		return source[index - at .. index];
	}

	bool eof() {
		return peekChar == 0;
	}

	string readWhile(bool delegate(dchar) predicate) {
		size_t start = index;
		while (!eof && predicate(peekChar)) {
			nextChar();
		}
		return source[start .. index];
	}

	void frontImpl() {
		Token result;

		readWhile(c => Chars.isSpace(c));

		size_t start = index;

		dchar first = peekChar;

		if (first == 0) {
			result.payload = Token.Eof();
		}
		else if (peekChars(2) == "//") {
			nextChars(2);
			dchar nestChar;
			int nestLevel;
			string value = readWhile((dchar c) {
				if (c == '(' || c == '{' || c == '[') {
					if (nestLevel == 0) {
						nestChar = c;
						nestLevel = 1;
					}
					else if (nestChar == c) {
						nestLevel += 1;
					}
				}
				if (c == ')' && nestChar == '(' && nestLevel > 0) {
					nestLevel -= 1;
				}
				if (c == '}' && nestChar == '{' && nestLevel > 0) {
					nestLevel -= 1;
				}
				if (c == ']' && nestChar == '[' && nestLevel > 0) {
					nestLevel -= 1;
				}
				return c != '\n' || nestLevel > 0;
			});
			result.payload = Token.Comment(value);
			return frontImpl; // TODO: do something with comments instead of throwing them away
		}
		else if (first == '"') {
			// FIXME: proper escapes and wysiwyg/hex/token/delimited strings
			nextChar();
			bool escape = false;
			string value = readWhile((c) {
				if (escape) {
					escape = false;
					return true;
				}
				else if (c == '\\') {
					escape = true;
					return true;
				}
				else if (c == '"') {
					return false;
				}

				return true;
			});
			if (eof) {
				throw new ParsingException(
					Diagnostic.Kind.Error,
					Nullable!Span(Span(source, index, index)),
					"unclosed string",
					"expected (\") here, not EOF",
					[
						Diagnostic.Extra(
							Diagnostic.Extra.Kind.Note,
							"string starts here",
							Nullable!Span(Span(source, start, start + 1)),
						),
					],
				);
			}
			nextChar();
			result.payload = Token.String(value);
		}
		else if (Chars.isDigit(first)) {
			// FIXME: all the types of integers
			ulong value = readWhile(c => Chars.isDigit(c)).to!ulong;
			result.payload = Token.Int(value);
		}
		// FIXME: char literals
		else if (Chars.isIdentifierStart(first)) {
			string value = readWhile(c => Chars.isIdentifier(c));
			ptrdiff_t kwId = KEYWORDS.countUntil(value);
			if (kwId != -1) {
				result.payload = Token.Keyword(KeywordId(cast(ushort) kwId));
			}
			else {
				result.payload = Token.Identifier(value);
			}
		}
		else {
			foreach (i, symbol; SYMBOLS) {
				if (peekChars(symbol.count) == symbol) {
					nextChars(symbol.count);
					result.payload = Token.Symbol(SymbolId(cast(ushort) i));
					goto foundOne;
				}
			}
			result.payload = Token.Unknown(nextChars(1));
			foundOne:
		}

		result.span = Span(source, start, index);
		peekedToken = result;
	}

public:

	Token front() {
		if (peekedToken.isNull)
			frontImpl();
		return peekedToken.get;
	}

	private Token last_;

	Token last() {
		return last_;
	}

	void popFront() {
		last_ = front;
		if (!empty)
			peekedToken = Nullable!Token.init;
	}

	bool empty() {
		return front.payload.match!((Token.Eof _) => true, _ => false);
	}

	Lexer save() {
		auto result = new Lexer;
		result.source = source;
		result.peekedToken = peekedToken;
		result.index = index;
		result.last_ = last_;
		return result;
	}

}

struct SToken(T) {
	Span span;
	T payload;
	alias payload this;
}

private template getPayloadContent(T) {
	T val;

	alias getPayloadContent = typeof(val.tupleof[0]);
}

SToken!T expect(T)(Lexer lexer) {
	SToken!T result;
	Token token = lexer.front;
	lexer.popFront();
	token.payload.match!(
		(T payload) {
			result.span = token.span;
			result.payload = payload;
		},
		(_) {
			throw new ParsingException(
				Diagnostic.Kind.Error,
				Nullable!Span(token.span),
				"expected " ~ T.stringof,
				"",
			);
		},
	);
	return result;
}

SToken!T expect(T)(Lexer lexer, getPayloadContent!T value) {
	SToken!T result;
	Token token = lexer.front;
	lexer.popFront();
	bool success = token.payload.match!(
		(T payload) {
			result.span = token.span;
			result.payload = payload;
			return payload.tupleof[0] == value;
		},
		(_) {
			return false;
		},
	);
	if (!success) {
		throw new ParsingException(
			Diagnostic.Kind.Error,
			Nullable!Span(token.span),
			"expected " ~ T.stringof ~ " '" ~ value.to!string ~ "'",
			"",
		);
	}
	return result;
}

bool isNext(T)(Lexer lexer) {
	Token token = lexer.front;
	return token.payload.match!(
		(T payload) {
			return true;
		},
		(_) {
			return false;
		},
	);
}

bool isNext(T)(Lexer lexer, getPayloadContent!T value) {
	Token token = lexer.front;
	return token.payload.match!(
		(T payload) {
			return payload.tupleof[0] == value;
		},
		(_) {
			return false;
		},
	);
}

bool tryNext(T)(Lexer lexer) {
	if (lexer.isNext!T) {
		lexer.popFront;
		return true;
	}
	else {
		return false;
	}
}

bool tryNext(T)(Lexer lexer, getPayloadContent!T value) {
	if (lexer.isNext!T(value)) {
		lexer.popFront;
		return true;
	}
	else {
		return false;
	}
}

bool until(T)(Lexer lexer) {
	return !lexer.empty && !lexer.isNext!T;
}

bool until(T)(Lexer lexer, getPayloadContent!T value) {
	return !lexer.empty && !lexer.isNext!T(value);
}

Span merge(Span from, Span to) {
	return Span(from.source, from.start, to.end);
}

struct InfixParselet {
	int precedence;
	bool delegate(Parser parser) isOperator;
	Expr delegate(Parser parser, Expr lhs) func;
}

private InfixParselet[] parselets;

static this() {
	parselets ~= InfixParselet(
		1,
		(Parser parser) { return parser.lexer.isNext!(Token.Symbol)(Symbol!"+"); },
		(Parser parser, Expr lhs) {
			parser.lexer.popFront;
			Expr rhs = parser.readExpr(1);

			BinaryExpr result = new BinaryExpr;
			result.op = BinaryOp.Add;
			result.lhs = lhs;
			result.rhs = rhs;
			result.span = merge(lhs.span, rhs.span);
			return result;
		},
	);
	parselets ~= InfixParselet(
		2,
		(Parser parser) { return parser.lexer.isNext!(Token.Symbol)(Symbol!"*"); },
		(Parser parser, Expr lhs) {
			parser.lexer.popFront;
			Expr rhs = parser.readExpr(2);

			BinaryExpr result = new BinaryExpr;
			result.op = BinaryOp.Mul;
			result.lhs = lhs;
			result.rhs = rhs;
			result.span = merge(lhs.span, rhs.span);
			return result;
		},
	);
	parselets ~= InfixParselet(
		3,
		(Parser parser) { return parser.lexer.isNext!(Token.Symbol)(Symbol!"("); },
		(Parser parser, Expr lhs) {
			parser.lexer.popFront;

			CallExpr result = new CallExpr;
			result.func = lhs;
			while (parser.lexer.until!(Token.Symbol)(Symbol!")")) {
				result.args ~= parser.readExpr;
				if (!parser.lexer.tryNext!(Token.Symbol)(Symbol!",")) {
					break;
				}
			}
			parser.lexer.expect!(Token.Symbol)(Symbol!")");
			result.span = merge(lhs.span, parser.lexer.last.span);
			return result;
		},
	);
	parselets ~= InfixParselet(
		100,
		(Parser parser) { return parser.lexer.isNext!(Token.Symbol)(Symbol!"!"); },
		(Parser parser, Expr lhs) {
			parser.lexer.popFront;

			TemplateCallExpr result = new TemplateCallExpr;
			result.templat = lhs;
			if (parser.lexer.tryNext!(Token.Symbol)(Symbol!"(")) {
				while (parser.lexer.until!(Token.Symbol)(Symbol!")")) {
					result.args ~= parser.readExpr;
					if (!parser.lexer.tryNext!(Token.Symbol)(Symbol!",")) {
						break;
					}
				}
				parser.lexer.expect!(Token.Symbol)(Symbol!")");
			}
			else {
				result.args ~= parser.readExpr(99);
			}
			result.span = merge(lhs.span, parser.lexer.last.span);
			return result;
		},
	);
}

final class Parser {

	private Lexer lexer;

	this(Source source) {
		lexer = new Lexer(source);
	}

	Span semi() {
		SToken!(Token.Symbol) token = lexer.expect!(Token.Symbol)(Symbol!";");
		return token.span;
	}

	Import readImport() {
		Import result = new Import;
		Span start = lexer.front.span;
		lexer.expect!(Token.Keyword)(Keyword!"import");

		while (lexer.until!(Token.Symbol)(Symbol!";")) {
			if (lexer.tryNext!(Token.Symbol)(Symbol!".")) {
				result.path ~= ".";
			}
			else if (lexer.tryNext!(Token.Symbol)(Symbol!"..")) {
				result.path ~= "..";
			}
			else {
				SToken!(Token.Identifier) ident = lexer.expect!(Token.Identifier);
				result.path ~= ident.name;
			}

			if (!lexer.tryNext!(Token.Symbol)(Symbol!"/")) {
				break;
			}
		}

		result.span = merge(start, semi);
		return result;
	}

	Program readProgram() {
		Program result = new Program;
		Span start = lexer.front.span;

		while (lexer.isNext!(Token.Keyword)(Keyword!"import")) {
			result.imports ~= readImport;
		}

		while (!lexer.empty) {
			result.decls ~= readDecl(true);
		}

		result.span = start;
		return result;
	}

	Decl readDecl(bool isGlobal = false) {
		Span start = lexer.front.span;

		if (lexer.tryNext!(Token.Keyword)(Keyword!"template")) {
			string name = lexer.expect!(Token.Identifier).name;

			Decl result = new Decl;
			result.isStatic = true;
			result.name = name;
			TemplateExpr templat = new TemplateExpr;
			lexer.expect!(Token.Symbol)(Symbol!"!");
			lexer.expect!(Token.Symbol)(Symbol!"(");
			while (lexer.until!(Token.Symbol)(Symbol!")")) {
				Expr paramType = readExpr;
				string paramName = lexer.expect!(Token.Identifier).name;
				templat.params ~= TemplateExpr.Param(paramType, paramName);
				if (!lexer.tryNext!(Token.Symbol)(Symbol!",")) {
					break;
				}
			}
			lexer.expect!(Token.Symbol)(Symbol!")");
			auto startCurly = lexer.expect!(Token.Symbol)(Symbol!"{");
			while (lexer.until!(Token.Symbol)(Symbol!"}")) {
				templat.body ~= readStat;
			}
			lexer.expect!(Token.Symbol)(Symbol!"}");

			result.initValue = templat;
			result.span = templat.span = merge(start, startCurly.span);

			templat.idName = name ~ "@" ~ templat.span.toString();

			return result;
		}

		bool isStatic;
		if (isGlobal) {
			isStatic = true;
		}
		else {
			if (lexer.tryNext!(Token.Keyword)(Keyword!"static")) {
				isStatic = true;
			}
		}

		Expr type = readExpr;
		string name = lexer.expect!(Token.Identifier).name;

		if (lexer.isNext!(Token.Symbol)(Symbol!"(")) {
			Decl result = new Decl;
			result.isStatic = isStatic;
			result.name = name;
			FuncExpr func = new FuncExpr;
			func.returnType = type;
			lexer.expect!(Token.Symbol)(Symbol!"(");
			while (lexer.until!(Token.Symbol)(Symbol!")")) {
				Expr paramType = readExpr;
				string paramName = lexer.expect!(Token.Identifier).name;
				func.params ~= FuncExpr.Param(paramType, paramName);
				if (!lexer.tryNext!(Token.Symbol)(Symbol!",")) {
					break;
				}
			}
			lexer.expect!(Token.Symbol)(Symbol!")");
			auto startCurly = lexer.expect!(Token.Symbol)(Symbol!"{");
			while (lexer.until!(Token.Symbol)(Symbol!"}")) {
				func.body ~= readStat;
			}
			lexer.expect!(Token.Symbol)(Symbol!"}");

			FunctionTypeExpr funcType = new FunctionTypeExpr;
			funcType.returnType = func.returnType;
			foreach (param; func.params) {
				funcType.paramTypes ~= param.type;
			}

			result.type = funcType;

			result.initValue = func;
			result.span = funcType.span = func.span = merge(start, startCurly.span);
			return result;
		}
		else {
			Decl result = new Decl;
			result.isStatic = isStatic;
			result.type = type;
			result.name = name;

			if (lexer.tryNext!(Token.Symbol)(Symbol!"=")) {
				result.initValue = readExpr;
				result.span = merge(start, semi);
			}
			else {
				NilLiteral initValue = new NilLiteral;
				result.initValue = initValue;
				result.span = initValue.span = merge(start, semi);
			}

			return result;
		}
	}

	Stat readStat() {
		Span start = lexer.front.span;
		if (lexer.tryNext!(Token.Keyword)(Keyword!"return")) {
			Return result = new Return;
			if (!lexer.isNext!(Token.Symbol)(Symbol!";")) {
				result.value = readExpr;
				result.span = merge(start, semi);
			}
			else {
				NilLiteral value = new NilLiteral;
				result.value = value;
				result.span = value.span = merge(start, semi);
			}
			return result;
		}

		Lexer saved = lexer.save();

		try {
			Expr expr = readExpr;
			if (CallExpr call = cast(CallExpr) expr) {
				ExprStat stat = new ExprStat;
				stat.value = expr;
				stat.span = merge(call.span, semi);
				return stat;
			}
		}
		catch (ParsingException) {}

		lexer = saved;
		return readDecl;
	}

	Expr readAtom() {
		Span start = lexer.front.span;
		if (lexer.isNext!(Token.Identifier)) {
			SToken!(Token.Identifier) token = lexer.expect!(Token.Identifier);
			VarAccess result = new VarAccess;
			result.span = token.span;
			result.name = token.name;
			return result;
		}
		else if (lexer.isNext!(Token.Int)) {
			SToken!(Token.Int) token = lexer.expect!(Token.Int);
			IntLiteral result = new IntLiteral;
			result.span = token.span;
			result.value = cast(int) token.value;
			return result;
		}
		else if (lexer.tryNext!(Token.Symbol)(Symbol!"(")) {
			Expr result = readExpr;
			lexer.expect!(Token.Symbol)(Symbol!")");
			result.span = merge(start, lexer.last.span);
			return result;
		}
		else if (lexer.tryNext!(Token.Keyword)(Keyword!"__builtin_value__")) {
			string value = lexer.expect!(Token.String).value;
			BuiltinValue result = new BuiltinValue;
			result.value = value;
			result.span = merge(start, lexer.last.span);
			return result;
		}
		else if (lexer.tryNext!(Token.Keyword)(Keyword!"nil")) {
			NilLiteral result = new NilLiteral;
			result.span = merge(start, lexer.last.span);
			return result;
		}
		else if (lexer.tryNext!(Token.Keyword)(Keyword!"type")) {
			TypeExpr result = new TypeExpr;
			result.span = start;
			return result;
		}
		else if (lexer.tryNext!(Token.Keyword)(Keyword!"function")) {
			FunctionTypeExpr result = new FunctionTypeExpr;
			lexer.expect!(Token.Symbol)(Symbol!"(");
			result.returnType = readExpr;
			if (lexer.tryNext!(Token.Symbol)(Symbol!";")) {
				while (lexer.until!(Token.Symbol)(Symbol!")")) {
					result.paramTypes ~= readExpr;
					if (!lexer.tryNext!(Token.Symbol)(Symbol!",")) {
						break;
					}
				}
			}
			lexer.expect!(Token.Symbol)(Symbol!")");
			result.span = merge(start, lexer.last.span);
			return result;
		}
		// else if (lexer.tryNext!(Token.Keyword)(Keyword!"const")) {
		// 	ConstExpr result = new ConstExpr;
		// 	lexer.expect!(Token.Symbol)(Symbol!"(");
		// 	result.inner = readExpr;
		// 	lexer.expect!(Token.Symbol)(Symbol!")");
		// 	result.span = merge(start, lexer.last.span);
		// 	return result;
		// }
		// else if (lexer.tryNext!(Token.Keyword)(Keyword!"immutable")) {
		// 	ImmutableExpr result = new ImmutableExpr;
		// 	lexer.expect!(Token.Symbol)(Symbol!"(");
		// 	result.inner = readExpr;
		// 	lexer.expect!(Token.Symbol)(Symbol!")");
		// 	result.span = merge(start, lexer.last.span);
		// 	return result;
		// }
		else {
			throw new ParsingException(
				Diagnostic.Kind.Error,
				Nullable!Span(start),
				"expected expression",
				"",
			);
		}
	}

	Expr readExpr(int precedence = 0) {
		Expr lhs = readAtom;
		outer: while (true) {
			foreach (parselet; parselets) {
				if (parselet.precedence > precedence && parselet.isOperator(this)) {
					lhs = parselet.func(this, lhs);
					continue outer;
				}
			}
			break;
		}
		return lhs;
	}

}
