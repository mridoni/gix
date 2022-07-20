using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("CONNECT TO :DATASRC USER :DBUSR USING :DBPWD")]
    //[Description("This tests the \"CONNECT TO :DATASRC USER :DBUSR USING :DBPWD\" variant of the CONNECT statement with the password supplied separately")]
    public class TSQL001 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }

        [TestMethod]
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        //[TestCategory("Using MSVC compilers"), TestCategory("Target DB: PGSQL"), TestCategory("Target arch: x64"), TestCategory("Build type: exe")]
        public void TSQL001A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            string ds = build_data_source_string(true, true, true);
            Environment.SetEnvironmentVariable("DATASRC", ds);
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }

        [TestMethod]
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        //[TestCategory("Using MSVC compilers"), TestCategory("Target DB: PGSQL"), TestCategory("Target arch: x64"), TestCategory("Build type: exe")]
        public void TSQL001A_MSVC_pgsql_x64_exe_with_encoding()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            string ds = build_data_source_string(true, true, true);
            ds += "?client_encoding=LATIN9";

            Environment.SetEnvironmentVariable("DATASRC", ds);
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }

        [TestMethod]
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("mysql", 1)]
        public void TSQL001A_MSVC_mysql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }

        [TestMethod]
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL001A_MSVC_pgsql_x86_exe()
        {
            compile(CompilerType.MSVC, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }


        [TestMethod]
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("mysql", 1)]
        public void TSQL001A_MSVC_mysql_x86_exe()
        {
            compile(CompilerType.MSVC, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }


        [TestMethod]
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("odbc", 1)]
        public void TSQL001A_MSVC_odbc_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe", false, false, "", "");

            string s = build_data_source_string(true, true, true);
            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT SQLCODE: +0000000000",
                    "SELECT SQLCODE: +000000000",
                    "RES: 003"
            });
        }

#if MINGW_TESTS_ENABLED
        [TestMethod]
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL001A_MinGW_pgsql_x64_exe()
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
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("mysql", 1)]
        public void TSQL001A_MinGW_mysql_x64_exe()
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
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL001A_MinGW_pgsql_x86_exe()
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
        [CobolSource("TSQL001A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("mysql", 1)]
        public void TSQL001A_MinGW_mysql_x86_exe()
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
