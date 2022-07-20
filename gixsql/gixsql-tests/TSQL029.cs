using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Support for DISCONNECT ALL (#89)")]
    public class TSQL029 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }

        // Standard
        [TestMethod]
        [CobolSource("TSQL029A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL029A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC1", build_data_source_string(false, true, true, 0));
            Environment.SetEnvironmentVariable("DATASRC_USR1", get_datasource_usr(0) + "." + get_datasource_pwd(0));
            Environment.SetEnvironmentVariable("DATASRC2", build_data_source_string(false, true, true, 1));
            Environment.SetEnvironmentVariable("DATASRC_USR2", get_datasource_usr(1) + "." + get_datasource_pwd(1));

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE(1): +0000000000",
                    "CONNECT SQLCODE(2): +0000000000",
                    "T1   : 0009",
                    "T2   : 0900",
                    "TOTAL: 0909",
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42P01",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42P01",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900",
                    "DISCONNECT ALL SQLCODE: +0000000000",
                    "SELECT-ERR 1 SQLCODE: -0000000103",
                    "SELECT-ERR 2 SQLCODE: -0000000103"
            });
        }

    }
}
