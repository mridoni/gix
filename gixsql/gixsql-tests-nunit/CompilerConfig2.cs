//using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;
using System.Xml;

namespace gixsql_tests
{

    public class CompilerConfig2
    {
        public string compiler_id { get; set; }

        public string gix_data_dir { get; set; }
        public string gix_bin_path { get; set; }
        public string gix_lib_path { get; set; }
        public string gix_copy_path { get; set; }

        public string cobc_homedir { get; set; }
        public string cobc_bin_dir_path { get; set; }
        public string cobc_lib_dir_path { get; set; }
        public string cobc_config_dir_path { get; set; }

        public string link_lib_dir_path { get; set; }
        public string link_lib_name { get; set; }
        public string link_lib_lname { get; set; }

        public string gixpp_exe { get; set; }
        public string cobc_exe { get; set; }
        public string cobcrun_exe { get; set; }
        public bool IsVsBased { get; set; }

        public static CompilerConfig2 init(string ctype, string architecture, string c_id)
        {
            try
            {
                CompilerConfig2 cc = new CompilerConfig2();

                string gix_base_path = Environment.GetEnvironmentVariable("GIXSQL_INSTALL_BASE");

                cc.compiler_id = c_id;

                string local_app_data = Environment.GetEnvironmentVariable("LOCALAPPDATA");
                cc.gix_data_dir = Path.Combine(local_app_data, "Gix");

                string cdef_file = Path.Combine(cc.gix_data_dir, "compiler-defs", cc.compiler_id + ".def");
                if (!File.Exists(cdef_file)) throw new Exception(cdef_file); ;

                XmlDocument xdef = new XmlDocument();
                xdef.Load(cdef_file);

                XmlElement xh = (XmlElement)xdef.SelectSingleNode($"//homedir");
                cc.cobc_homedir = xh.InnerText;

                XmlElement xp = (XmlElement)xdef.SelectSingleNode($"//platform[@id=\"{architecture}\"]");

                XmlElement xv = (XmlElement)xdef.SelectSingleNode($"//is_vs_based");
                cc.IsVsBased = Boolean.Parse(xv.InnerText);

                cc.cobc_bin_dir_path = xp.SelectSingleNode("bin_dir_path")?.InnerText;
                cc.cobc_bin_dir_path = cc.cobc_bin_dir_path.Replace("${homedir}", cc.cobc_homedir).Replace("${gixdata}", cc.gix_data_dir);
                if (!Directory.Exists(cc.cobc_bin_dir_path)) throw new Exception(cc.cobc_bin_dir_path);

                cc.cobc_lib_dir_path = xp.SelectSingleNode("lib_dir_path")?.InnerText;
                cc.cobc_lib_dir_path = cc.cobc_lib_dir_path.Replace("${homedir}", cc.cobc_homedir).Replace("${gixdata}", cc.gix_data_dir);
                if (!Directory.Exists(cc.cobc_lib_dir_path)) throw new Exception(cc.cobc_lib_dir_path);

                cc.cobc_config_dir_path = xp.SelectSingleNode("config_dir_path")?.InnerText;
                cc.cobc_config_dir_path = cc.cobc_config_dir_path.Replace("${homedir}", cc.cobc_homedir).Replace("${gixdata}", cc.gix_data_dir);
                if (!Directory.Exists(cc.cobc_config_dir_path)) throw new Exception(cc.cobc_config_dir_path);

                cc.gix_copy_path = Path.Combine(gix_base_path, "lib", "copy");
                if (!Directory.Exists(cc.gix_copy_path)) throw new Exception(cc.gix_copy_path);
                if (!File.Exists(Path.Combine(cc.gix_copy_path, "SQLCA.cpy"))) throw new Exception();

                cc.gix_bin_path = Path.Combine(gix_base_path, "bin");
                cc.gix_lib_path = Path.Combine(gix_base_path, "lib");
                cc.link_lib_dir_path = Path.Combine(cc.gix_lib_path, architecture, ctype);
                cc.link_lib_name = cc.IsVsBased ? "libgixsql.lib" : "libgixsql.a";
                if (!File.Exists(Path.Combine(cc.link_lib_dir_path, cc.link_lib_name))) throw new Exception(Path.Combine(cc.link_lib_dir_path, cc.link_lib_name));

                cc.gixpp_exe = Path.Combine(cc.gix_bin_path, "gixpp.exe");
                if (!File.Exists(cc.gixpp_exe)) throw new Exception(cc.gixpp_exe);

                cc.cobc_exe = Path.Combine(cc.cobc_bin_dir_path, "cobc.exe");
                if (!File.Exists(cc.cobc_exe)) throw new Exception(cc.cobc_exe);

                cc.cobcrun_exe = Path.Combine(cc.cobc_bin_dir_path, "cobcrun.exe");
                if (!File.Exists(cc.cobcrun_exe)) throw new Exception(cc.cobcrun_exe);

                cc.link_lib_lname = cc.IsVsBased ? "libgixsql" : "gixsql";

                return cc;
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message + "\n" + ex.StackTrace);
                throw ex;
            }
        }
    }
}
