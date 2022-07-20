using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("LOB related syntax issues (#3)")]
    public class TSQL014 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }


        [TestMethod]
        [CobolSource("TSQL014A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL014A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Console.WriteLine("Performed syntax check only: OK");
        }

        [TestMethod]
        [CobolSource("TSQL014B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL014B_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Console.WriteLine("Performed syntax check only: OK");
        }


        [TestMethod]
        [CobolSource("TSQL014C.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL014C_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Console.WriteLine("Performed syntax check only: OK");
        }

    }
}
