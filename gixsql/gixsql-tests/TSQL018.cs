using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Data.Common;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Long queries break COBOL compiler (#8)")]
    public class TSQL018 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();

            Environment.SetEnvironmentVariable("GIXSQL_DEBUG_LOG_ON", "1");
            Environment.SetEnvironmentVariable("GIXSQL_DEBUG_LOG", Path.Combine(TestTempDir, "gisql-debug.log"));
            Environment.SetEnvironmentVariable("GIXSQL_ERR_LOG", Path.Combine(TestTempDir, "gisql-error.log"));
        }

        [TestMethod]
        [CobolSource("TSQL018A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL018A_MSVC_pgsql_x64_exe_cobol85()
        {
            // "true" COBOL-85 is way too strict, we use "-std ibm" for testing
            compile(CompilerType.MSVC, "release", "x64", "exe", false, false, "--cobol85", "-std ibm");

            Console.WriteLine("Performed syntax check+compile only: OK");
        }

        [TestMethod]
        [CobolSource("TSQL018A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL018A_MSVC_pgsql_x64_exe_cobol2002()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe", false, false);

            Console.WriteLine("Performed syntax check+compile only: OK");
        }

        [TestMethod]
        [CobolSource("TSQL018B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL018B_MSVC_pgsql_x64_exe_cobol85()
        {
            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            compile(CompilerType.MSVC, "release", "x64", "exe", false, false, "--cobol85", "-std ibm");

            run(CompilerType.MSVC, "release", "x64", "exe");

            string[] lines = LastOutputText.Split(new string[] { "\r\n" }, StringSplitOptions.None);

            string sample1 = "LOREM IPSUM DOLOR SIT AMET , CONSECTETUR ADIPISCING ELIT . NUNC UT FRINGILLA ELIT . ALIQUAM AT URNA EU LIGULA FRINGILLA BIBENDUM . PROIN ACCUMSAN LOREM IMPERDIET , TINCIDUNT DUI UT , VESTIBULUM JUSTO . SUSPENDISSE SODALES IPSUM SAPIEN , NEC FEUGIAT EX VULPUTATE SCELERISQUE . VIVAMUS LOBORTIS JUSTO A FINIBUS ALIQUET . ETIAM A TELLUS SAPIEN . SUSPENDISSE MAXIMUS ID MAGNA A CURSUS . QUISQUE TINCIDUNT LACUS NON ENIM ULLAMCORPER TRISTIQUE";
            string sample2 = "METUS SED , INTERDUM MI . CRAS SIT AMET TEMPUS LIGULA . FUSCE FERMENTUM DUI AC ENIM FRINGILLA ULLAMCORPER EGET IN MASSA . ETIAM ET\0";
            string line = lines[lines.Length - 2];
            Assert.IsTrue(line.StartsWith(sample1));
            Assert.IsTrue(line.EndsWith(sample2));
        }

        [TestMethod]
        [CobolSource("TSQL018B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL018B_MSVC_pgsql_x64_exe_cobol2002()
        {
            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            compile(CompilerType.MSVC, "release", "x64", "exe", false, false);

            run(CompilerType.MSVC, "release", "x64", "exe");

            string[] lines = LastOutputText.Split(new string[] { "\r\n" }, StringSplitOptions.None);

            string sample1 = "LOREM IPSUM DOLOR SIT AMET , CONSECTETUR ADIPISCING ELIT . NUNC UT FRINGILLA ELIT . ALIQUAM AT URNA EU LIGULA FRINGILLA BIBENDUM . PROIN ACCUMSAN LOREM IMPERDIET , TINCIDUNT DUI UT , VESTIBULUM JUSTO . SUSPENDISSE SODALES IPSUM SAPIEN , NEC FEUGIAT EX VULPUTATE SCELERISQUE . VIVAMUS LOBORTIS JUSTO A FINIBUS ALIQUET . ETIAM A TELLUS SAPIEN . SUSPENDISSE MAXIMUS ID MAGNA A CURSUS . QUISQUE TINCIDUNT LACUS NON ENIM ULLAMCORPER TRISTIQUE";
            string sample2 = "METUS SED , INTERDUM MI . CRAS SIT AMET TEMPUS LIGULA . FUSCE FERMENTUM DUI AC ENIM FRINGILLA ULLAMCORPER EGET IN MASSA . ETIAM ET\0";
            string line = lines[lines.Length - 2];
            Assert.IsTrue(line.StartsWith(sample1));
            Assert.IsTrue(line.EndsWith(sample2));
        }

    }
}
