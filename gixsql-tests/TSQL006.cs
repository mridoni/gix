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
            const int DATA_SZ = 80;
            const int DATA_START = 33;  // starts with "!"

            byte[] data = null;

            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            //using (var conn = GetConnection())
            //{
            //    try
            //    {
            //        conn.Open();
            //        using (var cmd = conn.CreateCommand())
            //        {
            //            cmd.CommandText = "create table bintest (bfld bytea, vbfld bytea, cfld char(100), vcfld varchar(100), num1 numeric(4,2))"; // COBOL: bfld.size = 100, vbfld.size = 200
            //            cmd.ExecuteNonQuery();

            //            //data = Utils.RandomBytes(80);
            //            data = new byte[DATA_SZ];
            //            for (int i = 0; i < DATA_SZ; i++)
            //                data[i] = (byte)(DATA_START + i);

            //            cmd.CommandText = "insert into bintest (bfld, vbfld, cfld, vcfld, num1) values (@p_1, @p_2, 'ABCDEFGHIJK', 'ABCDEFGHIJK', 12.34)";
            //            DbParameter p_1 = cmd.CreateParameter();
            //            p_1.ParameterName = "@p_1";
            //            p_1.Value = data;
            //            DbParameter p_2 = cmd.CreateParameter();
            //            p_2.ParameterName = "@p_2";
            //            p_2.Value = data;
            //            cmd.Parameters.Add(p_1);
            //            cmd.Parameters.Add(p_2);
            //            cmd.ExecuteNonQuery();
            //        }
            //    }
            //    finally
            //    {
            //        if (conn != null && conn.State == System.Data.ConnectionState.Open)
            //            conn.Close();
            //    }

            //    int n = data.ToList().Sum(a => a);
            //}
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

        //[TestMethod]
        //[CobolSource("TSQL006A.cbl")]
        //[GixSqlDataSource("mysql", 1)]
        //public void TSQL006A_MSVC_mysql_x64_exe()
        //{
        //    compile(CompilerType.MSVC, "release", "x64", "exe");

        //    Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
        //    Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

        //    run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
        //            "employee #: [+0123]\r\nlast name : [Doe       ]\r\nfirst name: [John      ]\r\nstreet    : [123, Nowhere Lane   ]\r\ncity      : [Noplace        ]\r\nstate     : [NA]\r\nzip code  : [00100]\r\ndepartment: [DEP1]\r\npayrate   : [+0000000000200.00]\r\ncommission: [+1.23]\r\nmisc      : [abcd1234                                                                                                                        ]\r\nmisc (len): [+0008]\r\ndnum1     : [+00.00]\r\ndnum2     : [+22.33]\r\ndnum3     : [+33.44]",
        //            "employee #: [+0456]\r\nlast name : [Smith     ]\r\nfirst name: [Jane      ]\r\nstreet    : [456, Someplace Rd.  ]\r\ncity      : [Somewhere      ]\r\nstate     : [NA]\r\nzip code  : [00111]\r\ndepartment: [DEP2]\r\npayrate   : [+0000000000130.00]\r\ncommission: [+3.45]\r\nmisc      : [defg5678hijk                                                                                                                    ]\r\nmisc (len): [+0012]\r\ndnum1     : [+55.66]\r\ndnum2     : [+00.00]\r\ndnum3     : [+99.11]",
        //            "employee #: [+0789]\r\nlast name : [Smith2    ]\r\nfirst name: [Jane2     ]\r\nstreet    : [789, Someplace 2 Rd.]\r\ncity      : [Someplace Else ]\r\nstate     : [NA]\r\nzip code  : [00123]\r\ndepartment: [DEP3]\r\npayrate   : [+0000000000230.00]\r\ncommission: [+4.56]\r\nmisc      : [98005678hijk                                                                                                                    ]\r\nmisc (len): [+0012]\r\ndnum1     : [+12.34]\r\ndnum2     : [+45.67]\r\ndnum3     : [+00.00]",
        //            "fetch 00000100\r\nfetch +0000000100\r\nAll records in this table have been selected"

        //    });
        //}

        //[TestMethod]
        //[CobolSource("TSQL006A.cbl")]
        //[GixSqlDataSource("pgsql", 1)]
        //public void TSQL006A_MSVC_pgsql_x86_exe()
        //{
        //    compile(CompilerType.MSVC, "release", "x86", "exe");

        //    Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
        //    Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

        //    run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
        //            "employee #: [+0123]\r\nlast name : [Doe       ]\r\nfirst name: [John      ]\r\nstreet    : [123, Nowhere Lane   ]\r\ncity      : [Noplace        ]\r\nstate     : [NA]\r\nzip code  : [00100]\r\ndepartment: [DEP1]\r\npayrate   : [+0000000000200.00]\r\ncommission: [+1.23]\r\nmisc      : [abcd1234                                                                                                                        ]\r\nmisc (len): [+0008]\r\ndnum1     : [+00.00]\r\ndnum2     : [+22.33]\r\ndnum3     : [+33.44]",
        //            "employee #: [+0456]\r\nlast name : [Smith     ]\r\nfirst name: [Jane      ]\r\nstreet    : [456, Someplace Rd.  ]\r\ncity      : [Somewhere      ]\r\nstate     : [NA]\r\nzip code  : [00111]\r\ndepartment: [DEP2]\r\npayrate   : [+0000000000130.00]\r\ncommission: [+3.45]\r\nmisc      : [defg5678hijk                                                                                                                    ]\r\nmisc (len): [+0012]\r\ndnum1     : [+55.66]\r\ndnum2     : [+00.00]\r\ndnum3     : [+99.11]",
        //            "employee #: [+0789]\r\nlast name : [Smith2    ]\r\nfirst name: [Jane2     ]\r\nstreet    : [789, Someplace 2 Rd.]\r\ncity      : [Someplace Else ]\r\nstate     : [NA]\r\nzip code  : [00123]\r\ndepartment: [DEP3]\r\npayrate   : [+0000000000230.00]\r\ncommission: [+4.56]\r\nmisc      : [98005678hijk                                                                                                                    ]\r\nmisc (len): [+0012]\r\ndnum1     : [+12.34]\r\ndnum2     : [+45.67]\r\ndnum3     : [+00.00]",
        //            "fetch 00000100\r\nfetch +0000000100\r\nAll records in this table have been selected"

        //    });
        //}


        //[TestMethod]
        //[CobolSource("TSQL006A.cbl")]
        //[GixSqlDataSource("mysql", 1)]
        //public void TSQL006A_MSVC_mysql_x86_exe()
        //{
        //    compile(CompilerType.MSVC, "release", "x86", "exe");

        //    Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
        //    Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

        //    run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
        //            "employee #: [+0123]\r\nlast name : [Doe       ]\r\nfirst name: [John      ]\r\nstreet    : [123, Nowhere Lane   ]\r\ncity      : [Noplace        ]\r\nstate     : [NA]\r\nzip code  : [00100]\r\ndepartment: [DEP1]\r\npayrate   : [+0000000000200.00]\r\ncommission: [+1.23]\r\nmisc      : [abcd1234                                                                                                                        ]\r\nmisc (len): [+0008]\r\ndnum1     : [+00.00]\r\ndnum2     : [+22.33]\r\ndnum3     : [+33.44]",
        //            "employee #: [+0456]\r\nlast name : [Smith     ]\r\nfirst name: [Jane      ]\r\nstreet    : [456, Someplace Rd.  ]\r\ncity      : [Somewhere      ]\r\nstate     : [NA]\r\nzip code  : [00111]\r\ndepartment: [DEP2]\r\npayrate   : [+0000000000130.00]\r\ncommission: [+3.45]\r\nmisc      : [defg5678hijk                                                                                                                    ]\r\nmisc (len): [+0012]\r\ndnum1     : [+55.66]\r\ndnum2     : [+00.00]\r\ndnum3     : [+99.11]",
        //            "employee #: [+0789]\r\nlast name : [Smith2    ]\r\nfirst name: [Jane2     ]\r\nstreet    : [789, Someplace 2 Rd.]\r\ncity      : [Someplace Else ]\r\nstate     : [NA]\r\nzip code  : [00123]\r\ndepartment: [DEP3]\r\npayrate   : [+0000000000230.00]\r\ncommission: [+4.56]\r\nmisc      : [98005678hijk                                                                                                                    ]\r\nmisc (len): [+0012]\r\ndnum1     : [+12.34]\r\ndnum2     : [+45.67]\r\ndnum3     : [+00.00]",
        //            "fetch 00000100\r\nfetch +0000000100\r\nAll records in this table have been selected"

        //    });
        //}

#if MINGW_TESTS_ENABLED
        [TestMethod]
        [CobolSource("TSQL006A.cbl")]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL006A_MinGW_pgsql_x64_exe()
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
        [CobolSource("TSQL006A.cbl")]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL006A_MinGW_mysql_x64_exe()
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
        [CobolSource("TSQL006A.cbl")]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL006A_MinGW_pgsql_x86_exe()
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
        [CobolSource("TSQL006A.cbl")]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL006A_MinGW_mysql_x86_exe()
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
