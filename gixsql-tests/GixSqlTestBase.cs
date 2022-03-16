using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;
using System.Threading;
using Microsoft.Win32;
using System.Text;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml;
using System.Reflection;
using System.Diagnostics;
using CliWrap;
using CliWrap.Buffered;
using System.Threading.Tasks;
using System.Data.Common;
using Npgsql;
using MySql.Data.MySqlClient;
using System.Data.Odbc;

namespace gix_ide_tests
{
    public enum CompilerType
    {
        MSVC,
        MinGW
    }

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

    public class GixSqlTestBase
    {
        public TestContext TestContext { get; set; }

        protected string TestTempDir;
        protected string gix_base_path;

        protected Action __before_build = null;
        protected Action __after_build = null;

        protected Action __before_run = null;
        protected Action __after_run = null;

        protected Action __before_debug = null;
        protected Action __after_debug = null;

        protected bool flat_layout = false;

        protected string module_src;
        protected List<string> dependencies = new List<string>();

        //protected string data_source_type;
        //protected int data_source_index;

        private List<Tuple<string, int>> data_sources = new List<Tuple<string, int>>();

        private readonly Mutex testMutex = new Mutex(true, "my_mutex_test");


        [TestInitialize]
        public void Begin()
        {
            testMutex.WaitOne(TimeSpan.FromSeconds(20));

            System.Attribute[] attrs = System.Attribute.GetCustomAttributes(this.GetType());
            attrs = attrs.Where(a => a is HostPlatformAttribute).ToArray();

            if (attrs.Length != 1)
                Assert.Fail("Invalid test attributes (HostPlatform)");

            var pa = (HostPlatformAttribute)attrs[0];
            string platform = pa.Platform.ToUpper();
            gix_base_path = Environment.GetEnvironmentVariable($"GIX_BASE_PATH_{platform}");
            Assert.IsTrue(Directory.Exists(gix_base_path));
            Assert.IsTrue(Directory.Exists(Path.Combine(gix_base_path, "bin")));
            Assert.IsTrue(Directory.Exists(Path.Combine(gix_base_path, "lib")));

            if (!String.IsNullOrWhiteSpace(Environment.GetEnvironmentVariable("TEST_TEMP_DIR")))
                TestTempDir = Environment.GetEnvironmentVariable("TEST_TEMP_DIR");
            else
                TestTempDir = Path.Combine(Path.GetTempPath(), Utils.RandomString());

            Directory.CreateDirectory(TestTempDir);
            Console.WriteLine($"Test temporary directory: {TestTempDir}");

            MethodInfo methodInfo = GetType().GetMethod(TestContext.TestName);
            attrs = System.Attribute.GetCustomAttributes(methodInfo);
            attrs = attrs.Where(a => a is CobolSourceAttribute).ToArray();
            if (attrs.Length != 1)
                Assert.Fail("Invalid test attributes (CobolSource)");

            var ca = (CobolSourceAttribute)attrs[0];
            Utils.SaveResource(ca.Module, Path.Combine(TestTempDir, ca.Module));
            module_src = Path.Combine(TestTempDir, ca.Module);
            if (ca.Dependencies != null)
            {
                foreach (string d in ca.Dependencies)
                {
                    Utils.SaveResource(d, Path.Combine(TestTempDir, d));
                    dependencies.Add(Path.Combine(TestTempDir, d));
                }
            }

            attrs = System.Attribute.GetCustomAttributes(methodInfo);
            attrs = attrs.Where(a => a is GixSqlDataSourceAttribute).ToArray();
            //if (attrs.Length != 1)
            //    Assert.Fail("Invalid test attributes (CobolSource)");

            for (int i = 0; i < attrs.Length; i++){
                var ga = (GixSqlDataSourceAttribute)attrs[i];
                data_sources.Add(new Tuple<string, int>(ga.type, ga.index));
                Assert.IsTrue(data_source_init(data_sources.Count - 1));
            }
        }

        protected DbConnection GetConnection(int ds_index = 0)
        {
            DbConnection conn = null;
            string conn_string = null;
            var ds = data_sources[ds_index];

            string host = get_ds_val("host", ds_index);
            string port = get_ds_val("port", ds_index);
            string dbname = get_ds_val("dbname", ds_index);
            string usr = get_ds_val("usr", ds_index);
            string pwd = get_ds_val("pwd", ds_index);

            switch (ds.Item1.ToLower())
            {
                case "pgsql":
                    conn = new NpgsqlConnection();
                    conn_string = $"Host={host};Port={port};Database={dbname};Username={usr};Password={pwd}";
                    break;

                case "mysql":
                    conn_string = $"Host={host};Port={port};Database={dbname};Username={usr};Password={pwd}";
                    conn = new MySqlConnection();
                    break;

                case "odbc":
                    conn_string = $"DSN={host};UID={usr};PWD={pwd}";
                    conn = new OdbcConnection();
                    break;
            }

            Assert.IsNotNull(conn);

            conn.ConnectionString = conn_string;

            return conn;
        }

        private bool data_source_init(int ds_index)
        {
            DbConnection conn = GetConnection(ds_index);
            Assert.IsNotNull(conn);

            var ds = data_sources[ds_index];
            Assert.IsNotNull(ds);

            try
            {
                conn.Open();
                using (var cmd = conn.CreateCommand())
                {
                    string sql = Utils.GetResource("dbdata.sql");
                    Assert.IsFalse(String.IsNullOrWhiteSpace(sql));
                    cmd.CommandText = sql;
                    cmd.ExecuteNonQuery();
                }

                Console.WriteLine($"datasource {ds.Item1}/{ds.Item2} initialized");
                return true;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"datasource {ds.Item1}/{ds.Item2} initialization error: {ex.Message}");
                return false;
            }
            finally
            {
                if (conn != null && conn.State == System.Data.ConnectionState.Open)
                    conn.Close();
            }
        }

        [TestCleanup]
        public void End()
        {
            testMutex.ReleaseMutex();

            string kt = Environment.GetEnvironmentVariable("KEEP_TEMPS");
            if (kt != "1")
                Directory.Delete(TestTempDir, true);
        }

        protected void compile(CompilerType ctype, string configuration, string platform, string build_type)
        {
            string cwd = ".";
            string compiler_init_cmd = "break"; // break does nothing

            try
            {
                Assert.IsTrue(File.Exists(module_src));

                cwd = Environment.CurrentDirectory;
                Environment.CurrentDirectory = Path.GetDirectoryName(module_src);
                string msrc = Path.GetFileName(module_src);

                string pp_file = msrc.Replace(".cbl", ".cbsql");

                CompilerConfig cc = CompilerConfig.init(ctype, configuration, platform, build_type);

                // Preprocess
                string args = $"-e -v -S -I. -I{cc.gix_copy_path} -i {msrc} -o {pp_file}";
                Console.WriteLine($"[gixpp]: {cc.gixpp_exe} {args}");

                var r1 = Task.Run(async () =>
                {
                    return await Cli.Wrap(cc.gixpp_exe)
                         .WithArguments(args)
                         //.WithStandardOutputPipe(PipeTarget.ToStringBuilder(stdOutBuffer, System.Text.Encoding.ASCII))
                         //.WithStandardErrorPipe(PipeTarget.ToStringBuilder(stdErrBuffer, System.Text.Encoding.ASCII))
                         //.WithEnvironmentVariables(env)
                         .WithValidation(CommandResultValidation.None)
                         .ExecuteBufferedAsync();

                });

                Console.WriteLine(r1.Result.StandardOutput);
                Console.WriteLine(r1.Result.StandardError);
                Console.Out.Flush();

                Assert.IsTrue(r1.Result.ExitCode == 0, $"Exit code : {r1.Result.ExitCode:x}");
                Assert.IsTrue(File.Exists(pp_file));
                Assert.IsTrue((new FileInfo(pp_file)).Length > 0);

                // Compile

                if (ctype == CompilerType.MSVC)
                {
                    compiler_init_cmd = $@"{cc.cobc_bin_dir_path}\..\set_env_vs_{platform}.cmd";
                }
                Assert.IsTrue(File.Exists(cc.cobc_exe));

                Console.WriteLine($"[cobc]: {cc.cobc_exe}");

                string outfile = msrc.Replace(".cbl", "." + build_type);

                string opt_exe = build_type == "exe" ? "-x" : "";

                var r2 = Task.Run(async () =>
                {
                    return await Cli.Wrap("cmd.exe")
                       .WithArguments($"/C \"{compiler_init_cmd} && {cc.cobc_exe} {opt_exe} -I. -I{cc.gix_copy_path} {pp_file} -l{cc.link_lib_lname} -L{cc.link_lib_dir_path}")
                       //.WithStandardOutputPipe(PipeTarget.ToStringBuilder(stdOutBuffer))
                       //.WithStandardErrorPipe(PipeTarget.ToStringBuilder(stdErrBuffer))
                       .WithEnvironmentVariables(new Dictionary<string, string>
                       {
                           ["COB_CONFIG_DIR"] = cc.cobc_config_dir_path,
                           ["PATH"] = Environment.GetEnvironmentVariable("PATH") + $";{cc.cobc_bin_dir_path}"
                       })
                       .WithValidation(CommandResultValidation.None)
                       .ExecuteBufferedAsync();
                });

                Console.WriteLine(r2.Result.StandardOutput);
                Console.WriteLine(r2.Result.StandardError);

                Assert.IsTrue(r2.Result.ExitCode == 0, $"Exit code : {r2.Result.ExitCode}");
                Assert.IsTrue(File.Exists(outfile));
                FileInfo fi = new FileInfo(outfile);
                Assert.IsTrue(fi.Length > 0);

                Console.WriteLine($"Output: {fi.FullName} ({fi.Length} bytes)");
            }
            finally
            {
                Environment.CurrentDirectory = cwd;
            }
        }

        protected void run(CompilerType ctype, string configuration, string platform, string build_type,
                            string expected_md5_output_hash = "", bool useregex = false, string[] check_output_contains = null, int wait_runtime = 3000)
        {
            string cwd = ".";
            try
            {
                cwd = Environment.CurrentDirectory;
                Environment.CurrentDirectory = Path.GetDirectoryName(module_src);
                module_src = Path.GetFileName(module_src);

                string outfile = module_src.Replace(".cbl", "." + build_type);
                Assert.IsTrue(File.Exists(outfile));

                CompilerConfig cc = CompilerConfig.init(ctype, configuration, platform, build_type);

                string exe = String.Empty;

                Dictionary<string, string> env = new Dictionary<string, string>();
                string args = String.Empty;
                if (build_type == "exe") {
                    exe = outfile;
                    env["PATH"] = Environment.GetEnvironmentVariable("PATH") + $";{cc.cobc_bin_dir_path};{cc.link_lib_dir_path}";
                }
                else
                {
                    exe = cc.cobcrun_exe;
                    env["PATH"] = Environment.GetEnvironmentVariable("PATH") + $";{cc.cobc_bin_dir_path};{cc.link_lib_dir_path}";
                    args = module_src.Substring(0, module_src.IndexOf("."));
                }

                set_db_client_path(platform, env);

                Console.WriteLine($"Running {exe} {args}");

                var res = Task.Run(async () =>
                {
                    return await Cli.Wrap(exe)
                        .WithArguments(args)
                        //.WithStandardOutputPipe(PipeTarget.ToStringBuilder(stdOutBuffer))
                        //.WithStandardErrorPipe(PipeTarget.ToStringBuilder(stdErrBuffer))
                        .WithEnvironmentVariables(env)
                        .WithValidation(CommandResultValidation.None)
                        .ExecuteBufferedAsync();

                });
                
                Console.WriteLine(res.Result.StandardOutput);
                Console.WriteLine(res.Result.StandardError);

                Assert.IsTrue(res.Result.ExitCode == 0, $"Exit code : {res.Result.ExitCode}");

                Assert.IsFalse(String.IsNullOrWhiteSpace(res.Result.StandardOutput), "No output");

                bool b1 = false, b2 = false;

                if (!String.IsNullOrWhiteSpace(expected_md5_output_hash))
                {
                    string out_md5 = CreateMD5(File.ReadAllBytes(res.Result.StandardOutput));
                    Assert.AreEqual(expected_md5_output_hash, out_md5, $"Expected: {expected_md5_output_hash}, actual: {out_md5}");
                    b1 = true;
                }

                if (check_output_contains != null && check_output_contains.Length > 0)
                {
                    foreach (string t in check_output_contains)
                    {
                        if (useregex)
                        {
                            Regex rx = new Regex(t);
                            Assert.IsTrue(rx.IsMatch(res.Result.StandardOutput), "Output mismatch");
                        }
                        else {
                            Assert.IsTrue(res.Result.StandardOutput.Contains(t), "Output mismatch");
                        }
                        b2 = true;
                    }
                }

                if (b1 || b2)
                    Console.WriteLine("Output: OK");
                else
                    Assert.Inconclusive("Output not checked");

                if (__after_run != null)
                {
                    __after_run();
                }
            }

            finally
            {
                Environment.CurrentDirectory = cwd;
            }
        }

        private void set_db_client_path(string platform, Dictionary<string, string> env)
        {
            List<string> paths = new List<string>();
            
            foreach (var ds in data_sources.Select(a => a.Item1).Distinct()) {
                string v_id = $"{ds.ToUpper()}_CLIENT_PATH_{platform.ToUpper()}";
                string v_val = Environment.GetEnvironmentVariable(v_id);
                if (!String.IsNullOrWhiteSpace(v_val))
                    paths.Add(v_val);    
            }

            if (paths.Count > 0) {
                string path = env.ContainsKey("PATH") ? env["PATH"] : String.Empty;
                path += (";" + String.Join(";", paths));
                env["PATH"] = path;
            }
            
        }

        public static string CreateMD5(byte[] inputBytes)
        {
            // Use input string to calculate MD5 hash
            using (System.Security.Cryptography.MD5 md5 = System.Security.Cryptography.MD5.Create())
            {
                byte[] hashBytes = md5.ComputeHash(inputBytes);

                // Convert the byte array to hexadecimal string
                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < hashBytes.Length; i++)
                {
                    sb.Append(hashBytes[i].ToString("x2"));
                }
                return sb.ToString();
            }
        }

        protected string build_data_source_string(bool embed_auth, bool use_port, bool use_opts, int ds_index = 0)
        {
            Tuple<string, int> t = data_sources[ds_index];
            string data_source_type = t.Item1;
            int data_source_index = t.Item2;

            string s = data_source_type.ToLower() + "://";

            if (data_source_type == "odbc")
            {
                if (embed_auth)
                    s += $"{get_ds_val("usr", ds_index)}.{get_ds_val("pwd", ds_index)}@";

                s += get_ds_val("host", ds_index) + "/";
            }
            else {
                if (embed_auth)
                    s += $"{get_ds_val("usr", ds_index)}.{get_ds_val("pwd", ds_index)}@";

                s += get_ds_val("host", ds_index);

                if (use_port)
                    s += (":" + get_ds_val("port", ds_index));

                s += ("/" + get_ds_val("dbname", ds_index));

            }

            if (s.EndsWith("/"))
                s = s.Substring(0, s.Length - 1);

            if (use_opts)
            {
                string o = get_ds_val("opts", ds_index);
                if (!String.IsNullOrWhiteSpace(o))
                    s += ("?" + o);
            }
            
            return s;
        }

        private string get_ds_val(string base_key, int ds_index = 0)
        {
            string k = get_ds_key(base_key, ds_index);
            return Environment.GetEnvironmentVariable(k);
        }

        private string get_ds_key(string key, int ds_index = 0)
        {
            Tuple<string, int> t = data_sources[ds_index];
            string data_source_type = t.Item1;
            int data_source_index = t.Item2;

            return $"{data_source_type.ToUpper()}_{data_source_index}_{key.ToUpper()}";
        }

        protected string get_datasource_usr(int ds_index = 0)
        {
            return get_ds_val("usr", ds_index);
        }

        protected string get_datasource_pwd(int ds_index = 0)
        {
            return get_ds_val("pwd", ds_index);
        }
    }
}
