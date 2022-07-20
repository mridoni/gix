using gix_ide_tests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace gixsql_tests
{
    [TestClass]
    [HostPlatform("x64")]
    [TestCategory("CURSOR + misc data types")]
    public class TSQL004 : GixSqlTestBase
    {
        [TestInitialize]
        public new void Begin()
        {
            base.Begin();

        }


        [TestMethod]
        [CobolSource("TSQL004A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL004A_MSVC_pgsql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(false, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "employee #: [+0123]",
                    "last name : [Doe       ]",
                    "first name: [John      ]",
                    "street    : [123, Nowhere Lane   ]",
                    "city      : [Noplace        ]",
                    "state     : [NA]",
                    "zip code  : [00100]",
                    "department: [DEP1]",
                    "payrate   : [+0000000000200.00]",
                    "commission: [+1.23]",
                    "misc      : [abcd1234                                                                                                                        ]",
                    "misc (len): [0000000008]",
                    "dnum1     : [+00.00]",
                    "dnum2     : [+22.33]",
                    "dnum3     : [+33.44]",
                    "employee #: [+0456]",
                    "last name : [Smith     ]",
                    "first name: [Jane      ]",
                    "street    : [456, Someplace Rd.  ]",
                    "city      : [Somewhere      ]",
                    "state     : [NA]",
                    "zip code  : [00111]",
                    "department: [DEP2]",
                    "payrate   : [+0000000000130.00]",
                    "commission: [+3.45]",
                    "misc      : [defg5678hijk                                                                                                                    ]",
                    "misc (len): [0000000012]",
                    "dnum1     : [+55.66]",
                    "dnum2     : [+00.00]",
                    "dnum3     : [+99.11]",
                    "employee #: [+0789]",
                    "last name : [Smith2    ]",
                    "first name: [Jane2     ]",
                    "street    : [789, Someplace 2 Rd.]",
                    "city      : [Someplace Else ]",
                    "state     : [NA]",
                    "zip code  : [00123]",
                    "department: [DEP3]",
                    "payrate   : [+0000000000230.00]",
                    "commission: [+4.56]",
                    "misc      : [98005678hijk                                                                                                                    ]",
                    "misc (len): [0000000012]",
                    "dnum1     : [+12.34]",
                    "dnum2     : [+45.67]",
                    "dnum3     : [+00.00]",
                    "fetch 00000100",
                    "fetch +0000000100",
                    "All records in this table have been selected"
            });
        }

        [TestMethod]
        [CobolSource("TSQL004A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("mysql", 1)]
        public void TSQL004A_MSVC_mysql_x64_exe()
        {
            compile(CompilerType.MSVC, "release", "x64", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x64", "exe", "", false, new string[] {
                    "employee #: [+0123]",
                    "last name : [Doe       ]",
                    "first name: [John      ]",
                    "street    : [123, Nowhere Lane   ]",
                    "city      : [Noplace        ]",
                    "state     : [NA]",
                    "zip code  : [00100]",
                    "department: [DEP1]",
                    "payrate   : [+0000000000200.00]",
                    "commission: [+1.23]",
                    "misc      : [abcd1234                                                                                                                        ]",
                    "misc (len): [0000000008]",
                    "dnum1     : [+00.00]",
                    "dnum2     : [+22.33]",
                    "dnum3     : [+33.44]",
                    "employee #: [+0456]",
                    "last name : [Smith     ]",
                    "first name: [Jane      ]",
                    "street    : [456, Someplace Rd.  ]",
                    "city      : [Somewhere      ]",
                    "state     : [NA]",
                    "zip code  : [00111]",
                    "department: [DEP2]",
                    "payrate   : [+0000000000130.00]",
                    "commission: [+3.45]",
                    "misc      : [defg5678hijk                                                                                                                    ]",
                    "misc (len): [0000000012]",
                    "dnum1     : [+55.66]",
                    "dnum2     : [+00.00]",
                    "dnum3     : [+99.11]",
                    "employee #: [+0789]",
                    "last name : [Smith2    ]",
                    "first name: [Jane2     ]",
                    "street    : [789, Someplace 2 Rd.]",
                    "city      : [Someplace Else ]",
                    "state     : [NA]",
                    "zip code  : [00123]",
                    "department: [DEP3]",
                    "payrate   : [+0000000000230.00]",
                    "commission: [+4.56]",
                    "misc      : [98005678hijk                                                                                                                    ]",
                    "misc (len): [0000000012]",
                    "dnum1     : [+12.34]",
                    "dnum2     : [+45.67]",
                    "dnum3     : [+00.00]",
                    "fetch 00000100",
                    "fetch +0000000100",
                    "All records in this table have been selected"

            });
        }

        [TestMethod]
        [CobolSource("TSQL004A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 1)]
        public void TSQL004A_MSVC_pgsql_x86_exe()
        {
            compile(CompilerType.MSVC, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
                    "employee #: [+0123]",
                    "last name : [Doe       ]",
                    "first name: [John      ]",
                    "street    : [123, Nowhere Lane   ]",
                    "city      : [Noplace        ]",
                    "state     : [NA]",
                    "zip code  : [00100]",
                    "department: [DEP1]",
                    "payrate   : [+0000000000200.00]",
                    "commission: [+1.23]",
                    "misc      : [abcd1234                                                                                                                        ]",
                    "misc (len): [0000000008]",
                    "dnum1     : [+00.00]",
                    "dnum2     : [+22.33]",
                    "dnum3     : [+33.44]",
                    "employee #: [+0456]",
                    "last name : [Smith     ]",
                    "first name: [Jane      ]",
                    "street    : [456, Someplace Rd.  ]",
                    "city      : [Somewhere      ]",
                    "state     : [NA]",
                    "zip code  : [00111]",
                    "department: [DEP2]",
                    "payrate   : [+0000000000130.00]",
                    "commission: [+3.45]",
                    "misc      : [defg5678hijk                                                                                                                    ]",
                    "misc (len): [0000000012]",
                    "dnum1     : [+55.66]",
                    "dnum2     : [+00.00]",
                    "dnum3     : [+99.11]",
                    "employee #: [+0789]",
                    "last name : [Smith2    ]",
                    "first name: [Jane2     ]",
                    "street    : [789, Someplace 2 Rd.]",
                    "city      : [Someplace Else ]",
                    "state     : [NA]",
                    "zip code  : [00123]",
                    "department: [DEP3]",
                    "payrate   : [+0000000000230.00]",
                    "commission: [+4.56]",
                    "misc      : [98005678hijk                                                                                                                    ]",
                    "misc (len): [0000000012]",
                    "dnum1     : [+12.34]",
                    "dnum2     : [+45.67]",
                    "dnum3     : [+00.00]",
                    "fetch 00000100",
                    "fetch +0000000100",
                    "All records in this table have been selected"
            });
        }


        [TestMethod]
        [CobolSource("TSQL004A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("mysql", 1)]
        public void TSQL004A_MSVC_mysql_x86_exe()
        {
            compile(CompilerType.MSVC, "release", "x86", "exe");

            Environment.SetEnvironmentVariable("DATASRC", build_data_source_string(true, true, true));
            Environment.SetEnvironmentVariable("DATASRC_USR", get_datasource_usr() + "." + get_datasource_pwd());

            run(CompilerType.MSVC, "release", "x86", "exe", "", false, new string[] {
                    "employee #: [+0123]",
                    "last name : [Doe       ]",
                    "first name: [John      ]",
                    "street    : [123, Nowhere Lane   ]",
                    "city      : [Noplace        ]",
                    "state     : [NA]",
                    "zip code  : [00100]",
                    "department: [DEP1]",
                    "payrate   : [+0000000000200.00]",
                    "commission: [+1.23]",
                    "misc      : [abcd1234                                                                                                                        ]",
                    "misc (len): [0000000008]",
                    "dnum1     : [+00.00]",
                    "dnum2     : [+22.33]",
                    "dnum3     : [+33.44]",
                    "employee #: [+0456]",
                    "last name : [Smith     ]",
                    "first name: [Jane      ]",
                    "street    : [456, Someplace Rd.  ]",
                    "city      : [Somewhere      ]",
                    "state     : [NA]",
                    "zip code  : [00111]",
                    "department: [DEP2]",
                    "payrate   : [+0000000000130.00]",
                    "commission: [+3.45]",
                    "misc      : [defg5678hijk                                                                                                                    ]",
                    "misc (len): [0000000012]",
                    "dnum1     : [+55.66]",
                    "dnum2     : [+00.00]",
                    "dnum3     : [+99.11]",
                    "employee #: [+0789]",
                    "last name : [Smith2    ]",
                    "first name: [Jane2     ]",
                    "street    : [789, Someplace 2 Rd.]",
                    "city      : [Someplace Else ]",
                    "state     : [NA]",
                    "zip code  : [00123]",
                    "department: [DEP3]",
                    "payrate   : [+0000000000230.00]",
                    "commission: [+4.56]",
                    "misc      : [98005678hijk                                                                                                                    ]",
                    "misc (len): [0000000012]",
                    "dnum1     : [+12.34]",
                    "dnum2     : [+45.67]",
                    "dnum3     : [+00.00]",
                    "fetch 00000100",
                    "fetch +0000000100",
                    "All records in this table have been selected"
            });
        }

#if MINGW_TESTS_ENABLED
        [TestMethod]
        [CobolSource("TSQL004A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL004A_MinGW_pgsql_x64_exe()
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
        [CobolSource("TSQL004A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL004A_MinGW_mysql_x64_exe()
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
        [CobolSource("TSQL004A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("pgsql", 2)]
        public void TSQL004A_MinGW_pgsql_x86_exe()
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
        [CobolSource("TSQL004A.cbl", "EMPREC.cpy")]
        [GixSqlDataSource("mysql", 2)]
        public void TSQL004A_MinGW_mysql_x86_exe()
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
