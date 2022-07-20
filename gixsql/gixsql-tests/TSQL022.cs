using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Add support for VARYING groups (#38)")]
    public class TSQL022 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }


        // Standard
        [TestMethod]
        [CobolSource("TSQL022A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL022A_MSVC_pgsql_x64_exe_1()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe", false, false);

            check_file_contains(LastPreprocessedFile, new string[]
            {
                "GIXSQL*    01 VBFLD SQL TYPE IS VARBINARY(100).",
                "49 VBFLD-LEN PIC 9(8) COMP-5.",
                "49 VBFLD-ARR PIC X(100).",

                "GIXSQL*    01 VCFLD PIC X(100) VARYING.",
                "49 VCFLD-LEN PIC 9(8) COMP-5.",
                "49 VCFLD-ARR PIC X(100)."
            });
        }

        //Custom suffixes
        [TestMethod]
        [CobolSource("TSQL022A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL022A_MSVC_pgsql_x64_exe_2()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe", false, false, "--varying=LLLL,AAAA");

            check_file_contains(LastPreprocessedFile, new string[]
            {
                "GIXSQL*    01 VBFLD SQL TYPE IS VARBINARY(100).",
                "49 VBFLD-LLLL PIC 9(8) COMP-5.",
                "49 VBFLD-AAAA PIC X(100).",

                "GIXSQL*    01 VCFLD PIC X(100) VARYING.",
                "49 VCFLD-LLLL PIC 9(8) COMP-5.",
                "49 VCFLD-AAAA PIC X(100)."
            });
        }

        //Custom suffixes
        [TestMethod]
        [CobolSource("TSQL022B.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [TestCategory("Add support for VARYING groups (#38)")]
        [TestCategory("Checks for field length > 16 bit")]
        public void TSQL022B_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            check_file_contains(LastPreprocessedFile, new string[]
            {
                "01 BUFFER1.",
                "49 BUFFER1-LEN PIC 9(8) COMP-5.",
                "49 BUFFER1-ARR PIC X(100).",

                "01 BUFFER2.",
                "49 BUFFER2-LEN PIC 9(8) COMP-5.",
                "49 BUFFER2-ARR PIC X(32767).",

                "01 BUFFER3.",
                "49 BUFFER3-LEN PIC 9(8) COMP-5.",
                "49 BUFFER3-ARR PIC X(32768).",

                "01 BUFFER4.",
                "49 BUFFER4-LEN PIC 9(8) COMP-5.",
                "49 BUFFER4-ARR PIC X(50000).",

            });
        }

        [TestMethod]
        [CobolSource("TSQL022C.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        [TestCategory("Add support for VARYING groups (#38)")]
        [TestCategory("Checks for field length > 16 bit")]
        public void TSQL022C_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");
            
        }
    }
}
