using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Data.Common;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Length/power issues with COMP-3 (#92)")]
    public class TSQL019 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }


        [TestMethod]
        [CobolSource("TSQL019A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL019A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            using (var conn = GetConnection())
            {
                try
                {
                    conn.Open();
                    using (var cmd = conn.CreateCommand())
                    {
                        cmd.CommandText = "drop table if exists tab_a";
                        cmd.ExecuteNonQuery();

                        cmd.CommandText = "create table tab_a (id int, tornw numeric(30,12))";
                        cmd.ExecuteNonQuery();
                    }
                }
                finally
                {
                    if (conn != null && conn.State == System.Data.ConnectionState.Open)
                        conn.Close();
                }

            }

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "INSERT 1 SQLCODE: +0000000000",
                "INSERT 1 DATA   : -000000000000000042.740000000000",
                "INSERT 2 SQLCODE: +0000000000",
                "INSERT 2 DATA   : -000000000000000112",
                "INSERT 3 SQLCODE: +0000000000",
                "INSERT 3 DATA   : 000000000000000237",
                "INSERT 4 SQLCODE: +0000000000",
                "INSERT 4 DATA   : 000000000000000127.220000000000",
                "SELECT 1 SQLCODE: +0000000000",
                "SELECT 1 DATA   : -000000000000000042.740000000000",
                "SELECT 2 SQLCODE: +0000000000",
                "SELECT 2 DATA   : -000000000000000112",
                "SELECT 3 SQLCODE: +0000000000",
                "SELECT 3 DATA   : 000000000000000237",
                "SELECT 4 SQLCODE: +0000000000",
                "SELECT 4 DATA   : 000000000000000127.220000000000",
                "RES 1 : OK",
                "RES 2 : OK",
                "RES 3 : OK",
                "RES 4 : OK"
            });
        }


        [TestMethod]
        [CobolSource("TSQL019B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL019B_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            using (var conn = GetConnection())
            {
                try
                {
                    conn.Open();
                    using (var cmd = conn.CreateCommand())
                    {
                        cmd.CommandText = "drop table if exists tab_a";
                        cmd.ExecuteNonQuery();

                        cmd.CommandText = "create table tab_a (id int, tornw numeric(18,4))";
                        cmd.ExecuteNonQuery();
                    }
                }
                finally
                {
                    if (conn != null && conn.State == System.Data.ConnectionState.Open)
                        conn.Close();
                }

            }

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "INSERT 1 SQLCODE: +0000000000",
                "INSERT 1 DATA   : -000000000000000042.740000000000",
                "INSERT 2 SQLCODE: +0000000000",
                "INSERT 2 DATA   : -000000000000000112",
                "INSERT 3 SQLCODE: +0000000000",
                "INSERT 3 DATA   : 000000000000000237",
                "INSERT 4 SQLCODE: +0000000000",
                "INSERT 4 DATA   : 000000000000000127.220000000000",
                "SELECT 1 SQLCODE: +0000000000",
                "SELECT 1 DATA   : -000000000000000042.740000000000",
                "SELECT 2 SQLCODE: +0000000000",
                "SELECT 2 DATA   : -000000000000000112",
                "SELECT 3 SQLCODE: +0000000000",
                "SELECT 3 DATA   : 000000000000000237",
                "SELECT 4 SQLCODE: +0000000000",
                "SELECT 4 DATA   : 000000000000000127.220000000000",
                "RES 1 : OK",
                "RES 2 : OK",
                "RES 3 : OK",
                "RES 4 : OK"
            });
        }
    }
}
