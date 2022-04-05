using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Data.Common;
using System.IO;
using System.Linq;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Prepared statements")]
    public class TSQL006 : GixSqlTestBase
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
        [CobolSource("TSQL006A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL006A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "CONNECT DROP(1): +0000000000",
                "CONNECT CREATE(1): +0000000000",
                "PREPARE SQLSTMT1: +0000000000",
                "EXECUTE SQLSTMT1(1): +0000000000",
                "EXECUTE SQLSTMT1(2): +0000000000",
                "EXECUTE IMMEDIATE(1): +0000000000",
                "EXECUTE IMMEDIATE(2): +0000000000",
                "SUM SQLCODE: +0000000000",
                "TOT1: 0406",
                "TOT2: 1208"
            });
        }

        [TestMethod]
        [CobolSource("TSQL006B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [Description("Traps invalid variable for prepared statements")]
        public void TSQL006B_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe", true);

        }

        [TestMethod]
        [CobolSource("TSQL006C.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [Description("Traps invalid variable for prepared statements (correct format, should succeed)")]
        public void TSQL006C_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe", false, false);

        }


    }
}
