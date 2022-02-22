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
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617"
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
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617"
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
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617"
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
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617"
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
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617"
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
                    "SQLSTATE FAIL2 (OK IF <> 00000): 42617"
            });
        }


#if MINGW_TESTS_ENABLED
        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL003A_MinGW_pgsql_x64_exe()
        {
            compile(CompilerType.MinGW, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MinGW, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }

        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL003A_MinGW_mysql_x64_exe()
        {
            compile(CompilerType.MinGW, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MinGW, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }

        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL003A_MinGW_pgsql_x86_exe()
        {
            compile(CompilerType.MinGW, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MinGW, "release", "x86", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }

        [TestMethod]
        [CobolSource("TSQL003A.cbl")]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL003A_MinGW_mysql_x86_exe()
        {
            compile(CompilerType.MinGW, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MinGW, "release", "x86", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }
#endif

    }
}
