using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    //[TestCategory("COBOL TYPEDEF handling")]
    [TestCategory("Expected to fail: not implemented")]
    public class TSQL010 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }


        [TestMethod]
        [CobolSource("TSQL010A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        [Ignore]
        public void TSQL010A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Console.WriteLine("Performed syntax check only: OK");

        }

 
    }
}
