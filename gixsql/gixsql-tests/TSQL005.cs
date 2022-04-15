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
    public class TSQL005 : GixSqlTestBase
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
        [CobolSource("TSQL005A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [TestCategory("BINARY/VARBINARY data types")]
        public void TSQL005A_MSVC_pgsql_x64_exe()
        {
            const int DATA_SZ = 80;
            const int DATA_START = 33;  // starts with "!"

            byte[] data = null;

            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            using (var conn = GetConnection())
            {
                try
                {
                    conn.Open();
                    using (var cmd = conn.CreateCommand())
                    {
                        cmd.CommandText = "create table bintest (bfld bytea, vbfld bytea, cfld char(100), vcfld varchar(100), num1 numeric(4,2))"; // COBOL: bfld.size = 100, vbfld.size = 200
                        cmd.ExecuteNonQuery();

                        //data = Utils.RandomBytes(80);
                        data = new byte[DATA_SZ];
                        for (int i = 0; i < DATA_SZ; i++)
                            data[i] = (byte)(DATA_START + i);

                        cmd.CommandText = "insert into bintest (bfld, vbfld, cfld, vcfld, num1) values (@p_1, @p_2, 'ABCDEFGHIJK', 'ABCDEFGHIJK', 12.34)";
                        DbParameter p_1 = cmd.CreateParameter();
                        p_1.ParameterName = "@p_1";
                        p_1.Value = data;
                        DbParameter p_2 = cmd.CreateParameter();
                        p_2.ParameterName = "@p_2";
                        p_2.Value = data;
                        cmd.Parameters.Add(p_1);
                        cmd.Parameters.Add(p_2);
                        cmd.ExecuteNonQuery();
                    }
                }
                finally
                {
                    if (conn != null && conn.State == System.Data.ConnectionState.Open)
                        conn.Close();
                }

                int n = data.ToList().Sum(a => a);
            }
            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "DIGIT1 : +0033",
                "DIGIT2 : +0034",
                "DIGIT83: +0000",
                "BFLD        : [!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnop",
                "BFLD LENGTH: 100",
                "VBFLD-DATA  : [!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnop]",
                "VBFLD-LENGTH: 0080",
                "CFLD        : [ABCDEFGHIJK                                                                                         ]",
                "CFLD LENGTH: 100",
                "VCFLD-DATA  : [ABCDEFGHIJK]",
                "VCFLD-LENGTH: 0011"
            });
        }

        [TestMethod]
        [CobolSource("TSQL005A.cbl")]
        [GixSqlDataSource("mysql", 1)]
        [TestCategory("Expected to fail: not implemented")]
        public void TSQL005A_MSVC_mysql_x64_exe()
        {
            const int DATA_SZ = 80;
            const int DATA_START = 33;  // starts with "!"

            byte[] data = null;

            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            using (var conn = GetConnection())
            {
                try
                {
                    conn.Open();
                    using (var cmd = conn.CreateCommand())
                    {
                        cmd.CommandText = "create table bintest (bfld binary(200), vbfld varbinary(200), cfld char(100), vcfld varchar(100), num1 numeric(4,2))"; // COBOL: bfld.size = 100, vbfld.size = 200
                        cmd.ExecuteNonQuery();

                        //data = Utils.RandomBytes(80);
                        data = new byte[DATA_SZ];
                        for (int i = 0; i < DATA_SZ; i++)
                            data[i] = (byte)(DATA_START + i);

                        cmd.CommandText = "insert into bintest (bfld, vbfld, cfld, vcfld, num1) values (@p_1, @p_2, 'ABCDEFGHIJK', 'ABCDEFGHIJK', 12.34)";
                        DbParameter p_1 = cmd.CreateParameter();
                        p_1.ParameterName = "@p_1";
                        p_1.Value = data;
                        DbParameter p_2 = cmd.CreateParameter();
                        p_2.ParameterName = "@p_2";
                        p_2.Value = data;
                        cmd.Parameters.Add(p_1);
                        cmd.Parameters.Add(p_2);
                        cmd.ExecuteNonQuery();
                    }
                }
                finally
                {
                    if (conn != null && conn.State == System.Data.ConnectionState.Open)
                        conn.Close();
                }

                int n = data.ToList().Sum(a => a);
            }

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "DIGIT1 : +0033",
                "DIGIT2 : +0034",
                "DIGIT83: +0000",
                "BFLD        : [!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnop",
                "BFLD LENGTH: 100",
                "VBFLD-DATA  : [!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnop]",
                "VBFLD-LENGTH: 0080",
                "CFLD        : [ABCDEFGHIJK                                                                                         ]",
                "CFLD LENGTH: 100",
                "VCFLD-DATA  : [ABCDEFGHIJK]",
                "VCFLD-LENGTH: 0011"
            });
        }

        [TestMethod]
        [CobolSource("TSQL005A.cbl")]
        [GixSqlDataSource("odbc", 1)]
        [TestCategory("Expected to fail: not implemented")]
        public void TSQL005A_MSVC_odbc_x64_exe()
        {
            const int DATA_SZ = 80;
            const int DATA_START = 33;  // starts with "!"

            byte[] data = null;

            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            using (var conn = GetConnection())
            {
                try
                {
                    conn.Open();
                    using (var cmd = conn.CreateCommand())
                    {
                        cmd.CommandText = "create table bintest (bfld binary(200), vbfld varbinary(200), cfld char(100), vcfld varchar(100), num1 numeric(4,2))"; // COBOL: bfld.size = 100, vbfld.size = 200
                        cmd.ExecuteNonQuery();

                        //data = Utils.RandomBytes(80);
                        data = new byte[DATA_SZ];
                        for (int i = 0; i < DATA_SZ; i++)
                            data[i] = (byte)(DATA_START + i);

                        cmd.CommandText = "insert into bintest (bfld, vbfld, cfld, vcfld, num1) values (@p_1, @p_2, 'ABCDEFGHIJK', 'ABCDEFGHIJK', 12.34)";
                        DbParameter p_1 = cmd.CreateParameter();
                        p_1.ParameterName = "@p_1";
                        p_1.Value = data;
                        DbParameter p_2 = cmd.CreateParameter();
                        p_2.ParameterName = "@p_2";
                        p_2.Value = data;
                        cmd.Parameters.Add(p_1);
                        cmd.Parameters.Add(p_2);
                        cmd.ExecuteNonQuery();
                    }
                }
                finally
                {
                    if (conn != null && conn.State == System.Data.ConnectionState.Open)
                        conn.Close();
                }

                int n = data.ToList().Sum(a => a);
            }

            Assert.Inconclusive("Not implemented");

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "DIGIT1 : +0033",
                "DIGIT2 : +0034",
                "DIGIT83: +0000",
                "BFLD        : [!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnop",
                "BFLD LENGTH: 100",
                "VBFLD-DATA  : [!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnop]",
                "VBFLD-LENGTH: 0080",
                "CFLD        : [ABCDEFGHIJK                                                                                         ]",
                "CFLD LENGTH: 100",
                "VCFLD-DATA  : [ABCDEFGHIJK]",
                "VCFLD-LENGTH: 0011"
            });
        }

#if MINGW_TESTS_ENABLED
        [TestMethod]
        [CobolSource("TSQL005A.cbl")]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL005A_MinGW_pgsql_x64_exe()
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
        [CobolSource("TSQL005A.cbl")]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL005A_MinGW_mysql_x64_exe()
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
        [CobolSource("TSQL005A.cbl")]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL005A_MinGW_pgsql_x86_exe()
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
        [CobolSource("TSQL005A.cbl")]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL005A_MinGW_mysql_x86_exe()
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
