using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace gixsql_tests
{
    public class GixSqlTestData
    {
        public string Name;
        public string FullName => this.ToString();
        public string Architecture;
        public string CompilerType;

        public string Description;
        public string IssueCoverage;

        public List<List<string>> CobolModules = new List<List<string>>();
        public List<GixSqlTestDataSourceInfo> DataSources = new List<GixSqlTestDataSourceInfo>();
        public Dictionary<string, string> Environment = new Dictionary<string, string>();

        public bool Compile = false;
        public bool Run = false;
        public bool ExpectedToFailPreProcess = false;
        public bool ExpectedToFailCobc = false;
        public bool ExpectedToFailRun = false;

        public CompilerConfig2 CompilerConfiguration;
        
        public List<Tuple<int, string>> PreRunSQLFile = new List<Tuple<int, string>>();
        public List<Tuple<int, Tuple<string, List<string>>>> PreRunSQLStatement = new List<Tuple<int, Tuple<string, List<string>>>>();
        public List<Tuple<int, string>> PreRunDropTable = new List<Tuple<int, string>>();

        public Dictionary<string, string> GeneratedPayload = new Dictionary<string, string>();

        public List<string> ExpectedOutput = new List<string>();
        public List<string> ExpectedPreprocessedFileContent = new List<string>();

        public string LastPreprocessedFile;
        public string LastCompiledFile;

        public string LastErrorText;
        public string LastOutputText;

        public string BuildType = "exe";
        public string AdditionalPreProcessParams;
        public string AdditionalCompileParams;

        public override string ToString()
        {
            return $"({IssueCoverage}) - {Name}/{Architecture}/{CompilerType}/{DataSources[0].type} - {Description}";
        }

        internal void AddToEnvironment(string k, string v)
        {
            if (k == "PATH")
            {
                if (!this.Environment.ContainsKey(k))
                {
                    this.Environment[k] = v;
                }
                else
                {
                    this.Environment[k] = this.Environment[k] + ";" + v;
                }
            }
            else
            {
                this.Environment[k] = v;
            }
        }
    }
}
