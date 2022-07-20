using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Data.Common;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("CursorDeclareParams error (#50)")]
    public class TSQL017 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }

        [TestMethod]
        [CobolSource("TSQL017A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [TestCategory("EXEC IMMEDIATE/PREPARED + VARLEN error (#50)")]
        public void TSQL017A_MSVC_pgsql_x64_exe()
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

                        cmd.CommandText = "create table tab_a (col1 int, col2 int, key01 int)";
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
                "CONNECT SQLCODE: +0000000000",
                "PREPARE(1) SQLCODE: +0000000000",
                "EXECUTE SQLCODE: 000001+0000000000",
                "EXECUTE SQLCODE: 000002+0000000000",
                "EXECUTE SQLCODE: 000003+0000000000",
                "EXECUTE SQLCODE: 000004+0000000000",
                "EXECUTE SQLCODE: 000005+0000000000",
                "EXECUTE SQLCODE: 000006+0000000000",
                "EXECUTE SQLCODE: 000007+0000000000",
                "EXECUTE SQLCODE: 000008+0000000000",
                "EXECUTE SQLCODE: 000009+0000000000",
                "EXECUTE SQLCODE: 000010+0000000000",
                "SUM(1) SQLCODE: +0000000000",
                "TOT-KEY01(1): [000055]",
                "TOT-COL1 (1): [001055]",
                "TOT-COL2 (1): [002055]",
                "PREPARE(2) SQLCODE: +0000000000",
                "SUM(2) SQLCODE: +0000000000",
                "TOT-KEY01(2): [000036]",
                "TOT-COL1 (2): [000836]",
                "TOT-COL2 (2): [001636]"
            });
        }

        [TestMethod]
        [CobolSource("TSQL017B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL017B_MSVC_pgsql_x64_exe()
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

                        cmd.CommandText = "create table tab_a (col1 int, col2 int, key01 int)";
                        cmd.ExecuteNonQuery();

                        cmd.CommandText = "insert into tab_a (col1, col2, key01) values (@p_1, @p_2, @p_3)";

                        DbParameter p_1 = cmd.CreateParameter();
                        p_1.ParameterName = "@p_1";

                        DbParameter p_2 = cmd.CreateParameter();
                        p_2.ParameterName = "@p_2";

                        DbParameter p_3 = cmd.CreateParameter();
                        p_3.ParameterName = "@p_3";

                        cmd.Parameters.Add(p_1);
                        cmd.Parameters.Add(p_2);
                        cmd.Parameters.Add(p_3);

                        for (int i = 0; i < 15; i++)
                        {
                            p_1.Value = i + 100;
                            p_2.Value = i + 200;
                            p_3.Value = i;
                            cmd.ExecuteNonQuery();
                        }

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
                "OPEN SQLCODE: +0000000000"
            });
        }

        [TestMethod]
        [CobolSource("TSQL017C.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [TestCategory("EXEC PREPARED (using VARLEN field as source)")]
        public void TSQL017C_MSVC_pgsql_x64_exe()
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

                        cmd.CommandText = "create table tab_a (col1 int, col2 int, key01 int)";
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
                "CONNECT SQLCODE: +0000000000",
                "PREPARE SQLCODE: +0000000000",
                "EXECUTE SQLCODE: 000001+0000000000",
                "EXECUTE SQLCODE: 000002+0000000000",
                "EXECUTE SQLCODE: 000003+0000000000",
                "EXECUTE SQLCODE: 000004+0000000000",
                "EXECUTE SQLCODE: 000005+0000000000",
                "EXECUTE SQLCODE: 000006+0000000000",
                "EXECUTE SQLCODE: 000007+0000000000",
                "EXECUTE SQLCODE: 000008+0000000000",
                "EXECUTE SQLCODE: 000009+0000000000",
                "EXECUTE SQLCODE: 000010+0000000000",
                "SUM SQLCODE: +0000000000",
                "TOT-KEY01: [000055]",
                "TOT-COL1: [001055]",
                "TOT-COL2: [002055]"
            });
        }

    }
}
