import bread.source;
import bread.analysis;
import bread.parser;
import bread.ast;
import js = bread.backend.js;
import lua = bread.backend.lua;
import std;

void main() {
	Source source = new Source("test.bread", cast(string) read("test.bread"));
	Parser parser = new Parser(source);
	try {
		Program prog = parser.readProgram;
		auto ir = compile(prog);
		string compiled = lua.compile(ir);
		File file = File("test.toast.lua", "w");
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
