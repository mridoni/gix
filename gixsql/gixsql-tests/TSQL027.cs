using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("CURSOR smart init (#88)")]
    public class TSQL027 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }

        // Standard
        [TestMethod]
        [CobolSource("TSQL027A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL027A_MSVC_pgsql_x64_exe_1()
        {
            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            compile(CompilerType.MSVC, "release", "x64", "exe", false, false, "--smart-cursor-init");

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "VM1 - VAR-RES:00000010",
                "VM2 - VAR-RES:00000001",
                "VM3 - VAR-RES:00000009"
            });
        }



    }
}
