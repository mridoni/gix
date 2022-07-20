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
    [TestCategory("EXEC SQL WHENEVER")]
    public class TSQL024 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }


        [TestMethod]
        [CobolSource("TSQL024A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL024A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "CONNECT DROP(1): +0000000000",
                    "CONNECT CREATE(1): +0000000000",
                    "PREPARE SQLSTMT1: +0000000000",
                    "EXECUTE SQLSTMT1(1): +0000000000",
                    "EXECUTE SQLSTMT1(2): +0000000000",
                    "PRE-SEL-1   , DATA NOT FOUND FOR KEY = 0999",
                    "PRE-BAD-SELE, ERROR - SQLSTATE  : 42P01",
                    "PRE-SEL-2   , DATA NOT FOUND (2) FOR KEY = 0998",
                    "PRE-SEL-3   , ERROR (2) - SQLSTATE  : 42P01"
            });
        }


    }
}
