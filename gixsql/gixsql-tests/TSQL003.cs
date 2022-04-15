using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Multiple connections")]
    public class TSQL003 : GixSqlTestBase
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
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL003A_MSVC_pgsql_x64_exe()
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
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42617",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900"
            });
        }

        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("mysql", 1)]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL003A_MSVC_mysql_x64_exe()
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
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42617",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900"
            });
        }

        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL003A_MSVC_pgsql_x86_exe()
        {
            compile(CompilerType.MSVC, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC1", build_data_source_string(false, true, true, 0));
            Environment.SetEnvironmentVariable("DATASRC_USR1", get_datasource_usr(0) + "." + get_datasource_pwd(0));
            Environment.SetEnvironmentVariable("DATASRC2", build_data_source_string(false, true, true, 1));
            Environment.SetEnvironmentVariable("DATASRC_USR2", get_datasource_usr(1) + "." + get_datasource_pwd(1));

            run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
                    "CONNECT SQLCODE(1): +0000000000",
                    "CONNECT SQLCODE(2): +0000000000",
                    "T1   : 0009",
                    "T2   : 0900",
                    "TOTAL: 0909",
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42617",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900"
            });
        }


        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("mysql", 1)]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL003A_MSVC_mysql_x86_exe()
        {
            compile(CompilerType.MSVC, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC1", build_data_source_string(false, true, true, 0));
            Environment.SetEnvironmentVariable("DATASRC_USR1", get_datasource_usr(0) + "." + get_datasource_pwd(0));
            Environment.SetEnvironmentVariable("DATASRC2", build_data_source_string(false, true, true, 1));
            Environment.SetEnvironmentVariable("DATASRC_USR2", get_datasource_usr(1) + "." + get_datasource_pwd(1));

            run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
                    "CONNECT SQLCODE(1): +0000000000",
                    "CONNECT SQLCODE(2): +0000000000",
                    "T1   : 0009",
                    "T2   : 0900",
                    "TOTAL: 0909",
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42617",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900"
            });
        }

        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("mysql", 1)]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL003A_MSVC_mixed_x86_exe()
        {
            compile(CompilerType.MSVC, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC1", build_data_source_string(false, true, true, 0));
            Environment.SetEnvironmentVariable("DATASRC_USR1", get_datasource_usr(0) + "." + get_datasource_pwd(0));
            Environment.SetEnvironmentVariable("DATASRC2", build_data_source_string(false, true, true, 1));
            Environment.SetEnvironmentVariable("DATASRC_USR2", get_datasource_usr(1) + "." + get_datasource_pwd(1));

            run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
                    "CONNECT SQLCODE(1): +0000000000",
                    "CONNECT SQLCODE(2): +0000000000",
                    "T1   : 0009",
                    "T2   : 0900",
                    "TOTAL: 0909",
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42617",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900"
            });
        }

        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("mysql", 1)]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL003A_MSVC_mixed_x64_exe()
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
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42617",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900"
            });
        }


        // "B" test checks
        // EXEC SQL CONNECT :USERNAME IDENTIFIED BY :PASSWD AT :ATDBS USING :CONNECTSTRING END-EXEC. 
        [TestMethod]
        [CobolSource("TSQL003B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL003B_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC1", build_data_source_string(false, true, true, 0));
            Environment.SetEnvironmentVariable("DBID1", "CONN1");
            Environment.SetEnvironmentVariable("DATASRC_USR1", get_datasource_usr(0));
            Environment.SetEnvironmentVariable("DATASRC_PWD1", get_datasource_pwd(0));
            Environment.SetEnvironmentVariable("DATASRC2", build_data_source_string(false, true, true, 1));
            Environment.SetEnvironmentVariable("DATASRC_USR2", get_datasource_usr(1) + "." + get_datasource_pwd(1));

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE(1): +0000000000",
                    "CONNECT SQLCODE(2): +0000000000",
                    "T1   : 0009",
                    "T2   : 0900",
                    "TOTAL: 0909",
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42617",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900"
            });
        }

        // "C" test checks
        // Uses a field for connection IDs
        [TestMethod]
        [CobolSource("TSQL003C.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL003C_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC1", build_data_source_string(false, true, true, 0));
            Environment.SetEnvironmentVariable("DATASRC_USR1", get_datasource_usr(0));
            Environment.SetEnvironmentVariable("DATASRC_PWD1", get_datasource_pwd(0));
            Environment.SetEnvironmentVariable("DATASRC2", build_data_source_string(false, true, true, 1));
            Environment.SetEnvironmentVariable("DATASRC_USR2", get_datasource_usr(1) + "." + get_datasource_pwd(1));

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE(1): +0000000000",
                    "CONNECT SQLCODE(2): +0000000000",
                    "T1   : 0009",
                    "T2   : 0900",
                    "TOTAL: 0909",
                    "SQLSTATE FAIL1 (OK IF <> 00000): 42617",
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617",
                    "CRSR01 rec #0001 : [0001]",
                    "CRSR01 rec #0002 : [0003]",
                    "CRSR01 rec #0003 : [0005]",
                    "CRSR02 rec #0001 : [0100]",
                    "CRSR02 rec #0002 : [0300]",
                    "CRSR02 rec #0003 : [0500]",
                    "TOT CRSR01 :0009",
                    "TOT CRSR02 :0900"
            });
        }

    }
}
