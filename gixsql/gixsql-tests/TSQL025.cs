using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("Type/length calculation incomplete (#6)")]
    public class TSQL025 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();
        }

        // Standard
        [TestMethod]
        [CobolSource("TSQL025A.cbl")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL025A_MSVC_pgsql_x64_exe_1()
        {
            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr());
            Environment.SetEnvironmentVariable("DATASRC_PWD", get_datasource_pwd());

            compile(CompilerType.MSVC, "release", "x64", "exe", false, false);

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                "INSERT SQLCODE: +0000000000",
                "SELECT(*) SQLCODE: +0000000000",
                "CID(*)  : 000000000001",
                "FLD01(*): -0001",
                "FLD02(*): ABCD",
                "SELECT(ENUM) SQLCODE: +0000000000",
                "CID(ENUM)  : 000000000001",
                "FLD01(ENUM): -0001",
                "FLD02(ENUM): ABCD",
                "01 - INSERT SQLCODE: 01+0000000000",
                "02 - INSERT SQLCODE: 02+0000000000",
                "03 - INSERT SQLCODE: 03+0000000000",
                "04 - INSERT SQLCODE: 04+0000000000",
                "05 - INSERT SQLCODE: 05+0000000000",
                "06 - INSERT SQLCODE: 06+0000000000",
                "07 - INSERT SQLCODE: 07+0000000000",
                "08 - INSERT SQLCODE: 08+0000000000",
                "09 - INSERT SQLCODE: 09+0000000000",
                "10 - INSERT SQLCODE: 10+0000000000",
                "SQLCODE OPEN CRSR_TAB00 : +0000000000",
                "01 - FETCH SQLCODE: +0000000000",
                "01 - CID(FETCH)  : 000000000001",
                "01 - FLD01(FETCH): -0001",
                "01 - FLD02(FETCH): ABCD",
                "02 - FETCH SQLCODE: +0000000000",
                "02 - CID(FETCH)  : 000000000001",
                "02 - FLD01(FETCH): +0101",
                "02 - FLD02(FETCH): 0201",
                "03 - FETCH SQLCODE: +0000000000",
                "03 - CID(FETCH)  : 000000000002",
                "03 - FLD01(FETCH): +0102",
                "03 - FLD02(FETCH): 0202",
                "04 - FETCH SQLCODE: +0000000000",
                "04 - CID(FETCH)  : 000000000003",
                "04 - FLD01(FETCH): +0103",
                "04 - FLD02(FETCH): 0203",
                "05 - FETCH SQLCODE: +0000000000",
                "05 - CID(FETCH)  : 000000000004",
                "05 - FLD01(FETCH): +0104",
                "05 - FLD02(FETCH): 0204",
                "06 - FETCH SQLCODE: +0000000000",
                "06 - CID(FETCH)  : 000000000005",
                "06 - FLD01(FETCH): +0105",
                "06 - FLD02(FETCH): 0205",
                "07 - FETCH SQLCODE: +0000000000",
                "07 - CID(FETCH)  : 000000000006",
                "07 - FLD01(FETCH): +0106",
                "07 - FLD02(FETCH): 0206",
                "08 - FETCH SQLCODE: +0000000000",
                "08 - CID(FETCH)  : 000000000007",
                "08 - FLD01(FETCH): +0107",
                "08 - FLD02(FETCH): 0207",
                "09 - FETCH SQLCODE: +0000000000",
                "09 - CID(FETCH)  : 000000000008",
                "09 - FLD01(FETCH): +0108",
                "09 - FLD02(FETCH): 0208",
                "10 - FETCH SQLCODE: +0000000000",
                "10 - CID(FETCH)  : 000000000009",
                "10 - FLD01(FETCH): +0109",
                "10 - FLD02(FETCH): 0209",
                "11 - FETCH SQLCODE: +0000000000",
                "11 - CID(FETCH)  : 000000000010",
                "11 - FLD01(FETCH): +0110",
                "11 - FLD02(FETCH): 0210",
                "12 - FETCH SQLCODE: +0000000100",
                "12 - CID(FETCH)  : 000000000010",
                "12 - FLD01(FETCH): +0110",
                "12 - FLD02(FETCH): 0210"
            });
        }



    }
}
