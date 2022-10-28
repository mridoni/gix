using gixsql_tests;
using Microsoft.VisualStudio.TestPlatform.PlatformAbstractions.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Mono.Options;

namespace gixsql_tests_nunit
{
    public class Runner
    {
        static Dictionary<string, string> results = new Dictionary<string, string>();
        public static int Main(string[] args)
        {
            string runsettings = null;
            string test_filter = null;
            List<string> test_filter_list = new List<string>();

            var opts = new OptionSet() {
                { "r=", v => runsettings = v },
                { "f=", v => test_filter = v },
              };

            opts.Parse(args);

            DateTime start_time = DateTime.Now;

            if (String.IsNullOrWhiteSpace(runsettings))
            {
                if (Environment.GetEnvironmentVariable("GIXSQL_INSTALL_BASE") == null || !Directory.Exists(Environment.GetEnvironmentVariable("GIXSQL_INSTALL_BASE")))
                {
                    Console.Error.WriteLine("Invalid value for GIXSQL_INSTALL_BASE: " + Environment.GetEnvironmentVariable("GIXSQL_INSTALL_BASE"));
                    return 1;
                }

                if (Environment.GetEnvironmentVariable("KEEP_TEMPS") == null)
                {
                    Environment.SetEnvironmentVariable("KEEP_TEMPS", "0");
                }

                if (Environment.GetEnvironmentVariable("TEST_TEMP_DIR") == null)
                {
                    string tdir = Path.Combine(Path.GetTempPath(), Utils.RandomString());
                    Environment.SetEnvironmentVariable("TEST_TEMP_DIR", tdir);
                }
                else
                {
                    if (!Directory.Exists(Environment.GetEnvironmentVariable("TEST_TEMP_DIR")))
                    {
                        Console.Error.WriteLine("Invalid value for TEST_TEMP_DIR: " + Environment.GetEnvironmentVariable("TEST_TEMP_DIR"));
                        return 1;
                    }
                }

                Console.WriteLine("Using local environment");
                Console.WriteLine($"GIXSQL_INSTALL_BASE => {Environment.GetEnvironmentVariable("GIXSQL_INSTALL_BASE")}");
                Console.WriteLine($"KEEP_TEMPS          => {Environment.GetEnvironmentVariable("KEEP_TEMPS")}");
                Console.WriteLine($"TEST_TEMP_DIR       => {Environment.GetEnvironmentVariable("TEST_TEMP_DIR")}");

            }
            else
            {
                XmlDocument doc = new XmlDocument();
                doc.Load(args[0]);

                XmlElement xvr = (XmlElement)doc.SelectSingleNode("//EnvironmentVariables");

                foreach (XmlNode xn in xvr.ChildNodes)
                {
                    if (!(xn is XmlElement))
                        continue;

                    XmlElement xv = (XmlElement)xn;

                    Environment.SetEnvironmentVariable(xv.Name, xv.InnerText);
                    Console.WriteLine($"{xv.Name} => {xv.InnerText}");
                }
            }

            if (!String.IsNullOrWhiteSpace(test_filter))
            {
                foreach (var f in test_filter.Split(new String[] {",", ";"}, StringSplitOptions.RemoveEmptyEntries)) {
                    test_filter_list.Add(f);
                }
            }

            var tests = TestMatrixDataProvider.GetData();

            foreach (object[] t in tests)
            {
                GixSqlTestData test = (GixSqlTestData)t[0];

                if (test_filter_list.Count > 0 && !test_filter_list.Contains(test.Name)) {
                    Console.WriteLine("Skipping: " + test.FullName);
                    continue;
                }

                Console.WriteLine("Running: " + test.FullName);
                try
                {
                    var tr = new GixSqlDynamicTestRunner();
                    tr.Execute((GixSqlTestData)test);
                    results[test.FullName] = "OK";
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                    results[test.FullName] = "KO";
                }
            }

            int mlen = results.Select(a => a.Key.Length).Max();
            var orig_color = Console.ForegroundColor;
            foreach (var de in results)
            {
                Console.ForegroundColor = (de.Value == "OK") ? ConsoleColor.Green : ConsoleColor.Red;
                Console.WriteLine("{0}: {1}", de.Key.PadRight(mlen), de.Value);
                Console.ForegroundColor = orig_color;
            }

            Console.WriteLine("Run: {0} - Success: {1} - Failed: {2}",
                results.Count(),
                results.Where(a => a.Value == "OK").Count(),
                results.Where(a => a.Value == "KO").Count());


            DateTime end_time = DateTime.Now;

            var elapsed = end_time - start_time;

            Console.WriteLine("Elapsed:" + elapsed.ToString("c"));

            return 0;
        }
    }
}
