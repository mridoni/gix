using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Translation defect in 1.0.12 (#1)")]
    public class TSQL023 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();

            Environment.SetEnvironmentVariable("GIXSQL_DEBUG_LOG_ON", "1");
            Environment.SetEnvironmentVariable("GIXSQL_DEBUG_LOG", Path.Combine(TestTempDir, "gisql-debug.log"));
            Environment.SetEnvironmentVariable("GIXSQL_ERR_LOG", Path.Combine(TestTempDir, "gisql-error.log"));
        }


        // Standard
        [TestMethod]
        [CobolSource("TSQL023A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL023A_MSVC_pgsql_x64_exe_1()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe", false, false);

            check_file_contains(LastPreprocessedFile, new string[]
            {

            });
        }

        // Standard
        [TestMethod]
        [CobolSource("TSQL023B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL023B_MSVC_pgsql_x64_exe_1()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe", false, false);

            check_file_contains(LastPreprocessedFile, new string[]
            {

            });
        }


    }
}
