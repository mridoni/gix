using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("More formats for connect (#43)")]
    public class TSQL021 : GixSqlTestBase
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
        [CobolSource("TSQL021A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL021A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_FULL", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DBUSR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DBPWD", get_datasource_pwd());
            Environment.SetEnvironmentVariable("DBNAME", get_datasource_dbname());
            Environment.SetEnvironmentVariable("DBUSRPWD", get_datasource_usr() + "." + get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "CONNECT 1A SQLCODE: +0000000000",
                "CONNECT 2A SQLCODE: +0000000000",
                "CONNECT 3A SQLCODE: +0000000000",
                "CONNECT 4A SQLCODE: -0000000201",
                "CONNECT 5A SQLCODE: +0000000000",
                "CONNECT 6A SQLCODE: +0000000000",
                "CONNECT 1N SQLCODE: +0000000000",
                "CONNECT 2N SQLCODE: +0000000000",
                "CONNECT 3N SQLCODE: +0000000000",
                "CONNECT 4N SQLCODE: -0000000201"
            });
        }
    }
}
