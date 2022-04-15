using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Data.Common;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Long queries break COBOL compiler (#89)")]
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


    }
}
