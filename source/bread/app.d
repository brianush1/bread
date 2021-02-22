import bread.source;
import bread.analysis;
import bread.parser;
import bread.ast;
import bread.backend.js;
import std;

void main() {
	Source source = new Source("test.bread", cast(string) read("test.bread"));
	Parser parser = new Parser(source);
	try {
		Program prog = parser.readProgram;
		auto ir = compile(prog);
		string compiled = compile(ir);
		File file = File("test.toast.js", "w");
		file.write(compiled);
		file.close();
	}
	catch (ParsingException ex) {
		ex.diagnostic.report();
	}
	catch (AnalysisException ex) {
		ex.diagnostic.report();
	}
}
