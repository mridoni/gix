using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Data.Common;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Subselect (#66)")]
    public class TSQL012 : GixSqlTestBase
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
        [CobolSource("TSQL012A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL012A_MSVC_pgsql_x64_exe()
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

                        cmd.CommandText = "create table tab_a (histid int, tabrowid int, refnr int)"; 
                        cmd.ExecuteNonQuery();

                        cmd.CommandText = "insert into tab_a (histid, tabrowid, refnr) values (@p_1, @p_2, @p_3)";

                        DbParameter p_1 = cmd.CreateParameter();
                        p_1.ParameterName = "@p_1";
                 
                        DbParameter p_2 = cmd.CreateParameter();
                        p_2.ParameterName = "@p_2";

                        DbParameter p_3 = cmd.CreateParameter();
                        p_3.ParameterName = "@p_3";

                        cmd.Parameters.Add(p_1);
                        cmd.Parameters.Add(p_2);
                        cmd.Parameters.Add(p_3);

                        int rowid = 1;

                        int l = 2;
                        for (int i = 0; i < 5; i++){
                            p_1.Value = rowid++;
                            p_2.Value = i+ rowid;
                            p_3.Value = (l);
                            cmd.ExecuteNonQuery();
                        }
                        l++;

                        for (int i = 0; i < 5; i++)
                        {
                            p_1.Value = rowid++;
                            p_2.Value = i + rowid + 100;
                            p_3.Value = (l+1);
                            cmd.ExecuteNonQuery();
                        }
                        l++;

                        for (int i = 0; i < 5; i++)
                        {
                            p_1.Value = rowid++;
                            p_2.Value = i+ rowid + 200;
                            p_3.Value = (l);
                            cmd.ExecuteNonQuery();
                        }
                        l++;

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
                "SELECT SQLCODE: +0000000000",
                "RES: 00000220"
            });
        }

    }
}
