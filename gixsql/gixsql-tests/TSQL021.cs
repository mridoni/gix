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

        [TestMethod]
        [CobolSource("TSQL021B.cbl", "EMPREC.cpy")]
        [Description("Test multiple connection string formats")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL021B_MSVC_pgsql_x64_exe()
        {
            string datasrc, datasrc_full;

            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DBUSR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DBPWD", get_datasource_pwd());
            Environment.SetEnvironmentVariable("DBNAME", get_datasource_dbname());
            Environment.SetEnvironmentVariable("DBUSRPWD", get_datasource_usr() + "." + get_datasource_pwd());

            // 1 - Standard GixSQL connection string
            datasrc = $"pgsql://{get_datasource_host()}:{get_datasource_port()}/{get_datasource_dbname()}";
            datasrc_full = $"pgsql://{get_datasource_usr()}.{get_datasource_pwd()}@{get_datasource_host()}:{get_datasource_port()}/{get_datasource_dbname()}";
            Environment.SetEnvironmentVariable("DATASRC", datasrc);
            Environment.SetEnvironmentVariable("DATASRC_FULL", datasrc_full);

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

            // 2 - GixSQL connection string, no prefix (default driver)
            datasrc = $"{get_datasource_host()}:{get_datasource_port()}/{get_datasource_dbname()}";
            datasrc_full = $"{get_datasource_usr()}.{get_datasource_pwd()}@{get_datasource_host()}:{get_datasource_port()}/{get_datasource_dbname()}";
            Environment.SetEnvironmentVariable("DATASRC", datasrc);
            Environment.SetEnvironmentVariable("DATASRC_FULL", datasrc_full);
            Environment.SetEnvironmentVariable("GIXSQL_DEFAULT_DRIVER", get_datasource_type());

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
            Environment.SetEnvironmentVariable("GIXSQL_DEFAULT_DRIVER", null);

            // 3 - OCESQL-compatible connection string: [dbname][@host][:port]
            datasrc = $"{get_datasource_dbname()}@{get_datasource_host()}:{get_datasource_port()}";
            datasrc_full = String.Empty;
            Environment.SetEnvironmentVariable("DATASRC", datasrc);
            Environment.SetEnvironmentVariable("DATASRC_FULL", String.Empty);   // OCESQL does not support embedded authentication
            Environment.SetEnvironmentVariable("GIXSQL_DEFAULT_DRIVER", get_datasource_type());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "CONNECT 1A SQLCODE: +0000000000",
                "CONNECT 2A SQLCODE: +0000000000",
                "CONNECT 3A SQLCODE: +0000000000",
                "CONNECT 4A SQLCODE: -0000000201",
                "CONNECT 5A SQLCODE: -0000000201",  // 5A: embedded authentication, not supported in ocesql-compatible connection strings, expected to fail
                "CONNECT 6A SQLCODE: +0000000000",
                "CONNECT 1N SQLCODE: +0000000000",
                "CONNECT 2N SQLCODE: +0000000000",
                "CONNECT 3N SQLCODE: +0000000000",
                "CONNECT 4N SQLCODE: -0000000201"
            });
            Environment.SetEnvironmentVariable("GIXSQL_DEFAULT_DRIVER", null);
        }

        [TestMethod]
        [CobolSource("TSQL021C.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL021C_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", get_datasource_type() + "://" + get_datasource_host());
            Environment.SetEnvironmentVariable("DBUSR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DBPWD", get_datasource_pwd());
            Environment.SetEnvironmentVariable("DBNAME", get_datasource_dbname());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "CONNECT 6A SQLCODE: +0000000000",
            });

            Environment.SetEnvironmentVariable("GIXSQL_DEFAULT_DRIVER", get_datasource_type());
            Environment.SetEnvironmentVariable("DATASRC", get_datasource_host());
            Environment.SetEnvironmentVariable("DBUSR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DBPWD", get_datasource_pwd());
            Environment.SetEnvironmentVariable("DBNAME", get_datasource_dbname());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "CONNECT 6A SQLCODE: +0000000000",
            });
        }
    }
}
