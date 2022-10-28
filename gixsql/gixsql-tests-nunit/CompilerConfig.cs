//using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;
using System.Xml;

namespace gixsql_tests
{

    internal class CompilerConfig
    {
        public string compiler_id { get; private set; }

        public string gix_data_dir { get; private set; }
        public string gix_bin_path { get; private set; }
        public string gix_lib_path { get; private set; }
        public string gix_copy_path { get; private set; }

        public string cobc_homedir { get; private set; }
        public string cobc_bin_dir_path { get; private set; }
        public string cobc_lib_dir_path { get; private set; }
        public string cobc_config_dir_path { get; private set; }

        public string link_lib_dir_path { get; private set; }
        public string link_lib_name { get; private set; }
        public string link_lib_lname { get; private set; }

        public string gixpp_exe { get; private set; }
        public string cobc_exe { get; private set; }
        public string cobcrun_exe { get; private set; }

        public static CompilerConfig init(CompilerType ctype, string configuration, string platform, string build_type)
        {
            CompilerConfig cc = new CompilerConfig();

            string gix_base_path = Environment.GetEnvironmentVariable($"GIX_BASE_PATH_{platform}");

            if (ctype == CompilerType.MSVC)
            {
                cc.compiler_id = Environment.GetEnvironmentVariable("VC_COMPILER_ID");
            }
            else
            {
                cc.compiler_id = platform.ToLower() == "x86" ? Environment.GetEnvironmentVariable("MINGW_COMPILER_X86_ID") : Environment.GetEnvironmentVariable("MINGW_COMPILER_X64_ID");
            }

            string local_app_data = Environment.GetEnvironmentVariable("LOCALAPPDATA");
            cc.gix_data_dir = Path.Combine(local_app_data, "Gix");

            string cdef_file = Path.Combine(cc.gix_data_dir, "compiler-defs", cc.compiler_id + ".def");
            Assert.IsTrue(File.Exists(cdef_file));

            XmlDocument xdef = new XmlDocument();
            Assert.IsNotNull(xdef);
            xdef.Load(cdef_file);

            XmlElement xh = (XmlElement)xdef.SelectSingleNode($"//homedir");
            Assert.IsNotNull(xh);
            cc.cobc_homedir = xh.InnerText;

            XmlElement xp = (XmlElement)xdef.SelectSingleNode($"//platform[@id=\"{platform}\"]");
            Assert.IsNotNull(xp);

            cc.cobc_bin_dir_path = xp.SelectSingleNode("bin_dir_path")?.InnerText;
            cc.cobc_bin_dir_path = cc.cobc_bin_dir_path.Replace("${homedir}", cc.cobc_homedir).Replace("${gixdata}", cc.gix_data_dir);
            Assert.IsTrue(Directory.Exists(cc.cobc_bin_dir_path));

            cc.cobc_lib_dir_path = xp.SelectSingleNode("lib_dir_path")?.InnerText;
            cc.cobc_lib_dir_path = cc.cobc_lib_dir_path.Replace("${homedir}", cc.cobc_homedir).Replace("${gixdata}", cc.gix_data_dir);
            Assert.IsTrue(Directory.Exists(cc.cobc_lib_dir_path));

            cc.cobc_config_dir_path = xp.SelectSingleNode("config_dir_path")?.InnerText;
            cc.cobc_config_dir_path = cc.cobc_config_dir_path.Replace("${homedir}", cc.cobc_homedir).Replace("${gixdata}", cc.gix_data_dir);
            Assert.IsTrue(Directory.Exists(cc.cobc_config_dir_path));

            cc.gix_copy_path = Path.Combine(gix_base_path, "lib", "copy");
            Assert.IsTrue(Directory.Exists(cc.gix_copy_path));
            Assert.IsTrue(File.Exists(Path.Combine(cc.gix_copy_path, "SQLCA.cpy")));

            cc.gix_bin_path = Path.Combine(gix_base_path, "bin");
            cc.gix_lib_path = Path.Combine(gix_base_path, "lib");
            cc.link_lib_dir_path = Path.Combine(cc.gix_lib_path, platform, (ctype == CompilerType.MSVC ? "msvc" : "gcc"));
            cc.link_lib_name = ctype == CompilerType.MSVC ? "libgixsql.lib" : "libgixsql.a";
            Assert.IsTrue(File.Exists(Path.Combine(cc.link_lib_dir_path, cc.link_lib_name)));

            cc.gixpp_exe = Path.Combine(cc.gix_bin_path, "gixpp.exe");
            Assert.IsTrue(File.Exists(cc.gixpp_exe));

            cc.cobc_exe = Path.Combine(cc.cobc_bin_dir_path, "cobc.exe");
            Assert.IsTrue(File.Exists(cc.cobc_exe));

            cc.cobcrun_exe = Path.Combine(cc.cobc_bin_dir_path, "cobcrun.exe");
            Assert.IsTrue(File.Exists(cc.cobcrun_exe));

            cc.link_lib_lname = ctype == CompilerType.MSVC ? "libgixsql" : "gixsql";

            return cc;
        }
    }
}
