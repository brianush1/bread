import bread.source;
import bread.analysis;
import bread.parser;
import bread.ast;
import std;

void main() {
	Source source = new Source("test.bread", cast(string) read("test.bread"));
	Parser parser = new Parser(source);
	try {
		Program prog = parser.readProgram;
		Environment env = analyze(prog);
		Value v = env.staticVars["main"].value;
		writeln(v.operate(Operation.Call, []));
	}
	catch (ParsingException ex) {
		ex.diagnostic.report();
	}
	catch (AnalysisException ex) {
		ex.diagnostic.report();
	}
}
