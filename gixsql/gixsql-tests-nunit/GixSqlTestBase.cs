////using Microsoft.VisualStudio.TestTools.UnitTesting;
//using System;
//using System.IO;
//using System.Threading;
//using Microsoft.Win32;
//using System.Text;
//using System.Linq;
//using System.Collections.Generic;
//using System.Text.RegularExpressions;
//using System.Reflection;
//using System.Diagnostics;
//using CliWrap;
//using CliWrap.Buffered;
//using System.Threading.Tasks;
//using System.Data.Common;
//using Npgsql;
//using MySql.Data.MySqlClient;
//using System.Data.Odbc;

//namespace gixsql_tests
//{

//    public class GixSqlTestBase
//    {
//        public TestContext TestContext { get; set; }

//        protected string TestTempDir;
//        protected string gix_base_path;

//        protected Action __before_build = null;
//        protected Action __after_build = null;

//        protected Action __before_run = null;
//        protected Action __after_run = null;

//        protected Action __before_debug = null;
//        protected Action __after_debug = null;

//        protected bool flat_layout = false;

//        protected List<string> module_srcs = new List<string>();
//        protected List<string> dependencies = new List<string>();

//        protected string LastPreprocessedFile { get { return last_preprocessed_file; } }
//        protected string LastCompiledFile { get { return last_compiled_file; } }
        
//        protected string LastOutputText { get { return last_output_text; } }
//        protected string LastErrorText { get { return last_error_text; } }
//        protected double LastExecutionTime { get { return execution_time; } }


//        private string last_preprocessed_file;
//        private string last_compiled_file;        
        
//        private string last_error_text;
//        private string last_output_text;

//        private double execution_time;

//        private List<Tuple<string, int>> data_sources = new List<Tuple<string, int>>();

//        private readonly Mutex testMutex = new Mutex(true, "my_mutex_test");


//        [SetUp]
//        public void Begin()
//        {
//            testMutex.WaitOne(TimeSpan.FromSeconds(20));

//            System.Attribute[] attrs = System.Attribute.GetCustomAttributes(this.GetType());
//            attrs = attrs.Where(a => a is HostPlatformAttribute).ToArray();

//            if (attrs.Length != 1)
//                Assert.Fail("Invalid test attributes (HostPlatform)");

//            var pa = (HostPlatformAttribute)attrs[0];
//            string platform = pa.Platform.ToUpper();
//            gix_base_path = Environment.GetEnvironmentVariable($"GIX_BASE_PATH_{platform}");
//            Assert.IsTrue(Directory.Exists(gix_base_path));
//            Assert.IsTrue(Directory.Exists(Path.Combine(gix_base_path, "bin")));
//            Assert.IsTrue(Directory.Exists(Path.Combine(gix_base_path, "lib")));

//            if (!String.IsNullOrWhiteSpace(Environment.GetEnvironmentVariable("TEST_TEMP_DIR")))
//                TestTempDir = Environment.GetEnvironmentVariable("TEST_TEMP_DIR");
//            else
//                TestTempDir = Path.Combine(Path.GetTempPath(), Utils.RandomString());

//            Directory.CreateDirectory(TestTempDir);
//            Console.WriteLine($"Test temporary directory: {TestTempDir}");

//            MethodInfo methodInfo = GetType().GetMethod(TestContext.TestName);
//            attrs = System.Attribute.GetCustomAttributes(methodInfo);
//            attrs = attrs.Where(a => a is CobolSourceAttribute).ToArray();

//            for (int i = 0; i < attrs.Length; i++)
//            {
//                var ca = (CobolSourceAttribute)attrs[i];
//                Utils.SaveResource(ca.Module, Path.Combine(TestTempDir, ca.Module));
//                string m = Path.Combine(TestTempDir, ca.Module);
//                module_srcs.Add(m);
//                if (ca.Dependencies != null)
//                {
//                    foreach (string d in ca.Dependencies)
//                    {
//                        Utils.SaveResource(d, Path.Combine(TestTempDir, d));
//                        dependencies.Add(Path.Combine(TestTempDir, d));
//                    }
//                }
//            }

//            attrs = System.Attribute.GetCustomAttributes(methodInfo);
//            attrs = attrs.Where(a => a is GixSqlDataSourceAttribute).ToArray();
//            //if (attrs.Length != 1)
//            //    Assert.Fail("Invalid test attributes (CobolSource)");

//            for (int i = 0; i < attrs.Length; i++)
//            {
//                var ga = (GixSqlDataSourceAttribute)attrs[i];
//                data_sources.Add(new Tuple<string, int>(ga.type, ga.index));
//                Assert.IsTrue(data_source_init(data_sources.Count - 1));
//            }

//            string log_path = Path.Combine(TestTempDir, "gixsql-" + this.GetType().Name + ".log");
//            if (File.Exists(log_path))
//                File.Delete(log_path);

//            Environment.SetEnvironmentVariable("GIXSQL_LOG_LEVEL", "trace");
//            Environment.SetEnvironmentVariable("GIXSQL_LOG_FILE", log_path);
//        }

//        protected DbConnection GetConnection(int ds_index = 0)
//        {
//            DbConnection conn = null;
//            string conn_string = null;
//            var ds = data_sources[ds_index];

//            string host = get_ds_val("host", ds_index);
//            string port = get_ds_val("port", ds_index);
//            string dbname = get_ds_val("dbname", ds_index);
//            string usr = get_ds_val("usr", ds_index);
//            string pwd = get_ds_val("pwd", ds_index);

//            switch (ds.Item1.ToLower())
//            {
//                case "pgsql":
//                    conn = new NpgsqlConnection();
//                    conn_string = $"Host={host};Port={port};Database={dbname};Username={usr};Password={pwd}";
//                    break;

//                case "mysql":
//                    conn_string = $"Host={host};Port={port};Database={dbname};Username={usr};Password={pwd}";
//                    conn = new MySqlConnection();
//                    break;

//                case "odbc":
//                    conn_string = $"DSN={host};UID={usr};PWD={pwd}";
//                    conn = new OdbcConnection();
//                    break;
//            }

//            Assert.IsNotNull(conn);

//            conn.ConnectionString = conn_string;

//            return conn;
//        }

//        private bool data_source_init(int ds_index)
//        {
//            DbConnection conn = GetConnection(ds_index);
//            Assert.IsNotNull(conn);

//            var ds = data_sources[ds_index];
//            Assert.IsNotNull(ds);

//            try
//            {
//                conn.Open();
//                using (var cmd = conn.CreateCommand())
//                {
//                    string sql = Utils.GetResource("dbdata.sql");
//                    Assert.IsFalse(String.IsNullOrWhiteSpace(sql));
//                    cmd.CommandText = sql;
//                    cmd.ExecuteNonQuery();
//                }

//                Console.WriteLine($"datasource {ds.Item1}/{ds.Item2} initialized");
//                return true;
//            }
//            catch (Exception ex)
//            {
//                Console.WriteLine($"datasource {ds.Item1}/{ds.Item2} initialization error: {ex.Message}");
//                return false;
//            }
//            finally
//            {
//                if (conn != null && conn.State == System.Data.ConnectionState.Open)
//                    conn.Close();
//            }
//        }

//        [TearDown]
//        public void End()
//        {
//            testMutex.ReleaseMutex();

//            string kt = Environment.GetEnvironmentVariable("KEEP_TEMPS");
//            if (kt != "1")
//                Directory.Delete(TestTempDir, true);
//        }

//        protected void compile(CompilerType ctype, string configuration, string platform, string build_type,
//                                bool expected_to_fail_pp = false, bool expected_to_fail_cobc = false,
//                                string additional_pp_params = "", string additional_cobc_params = "")
//        {
//            string cwd = ".";
//            string compiler_init_cmd = "break"; // break does nothing

//            try
//            {
//                foreach (string module_src in module_srcs)
//                {
//                    Assert.IsTrue(File.Exists(module_src));

//                    cwd = Environment.CurrentDirectory;
//                    Environment.CurrentDirectory = Path.GetDirectoryName(module_src);
//                    string msrc = Path.GetFileName(module_src);

//                    string pp_file = msrc.Replace(".cbl", ".cbsql");

//                    CompilerConfig cc = CompilerConfig.init(ctype, configuration, platform, build_type);

//                    // Preprocess
//                    string gixpp_args = $"-e -v -S -I. -I{cc.gix_copy_path} -i {msrc} -o {pp_file}";
//                    if (additional_pp_params != "")
//                        gixpp_args += (" " + additional_pp_params);

//                    Console.WriteLine($"[gixpp]: {cc.gixpp_exe} {gixpp_args}");

//                    var r1 = Task.Run(async () =>
//                    {
//                        return await Cli.Wrap(cc.gixpp_exe)
//                             .WithArguments(gixpp_args)
//                             //.WithStandardOutputPipe(PipeTarget.ToStringBuilder(stdOutBuffer, System.Text.Encoding.ASCII))
//                             //.WithStandardErrorPipe(PipeTarget.ToStringBuilder(stdErrBuffer, System.Text.Encoding.ASCII))
//                             //.WithEnvironmentVariables(env)
//                             .WithValidation(CommandResultValidation.None)
//                             .ExecuteBufferedAsync();

//                    });

//                    Console.WriteLine(r1.Result.StandardOutput);
//                    Console.WriteLine(r1.Result.StandardError);
//                    Console.Out.Flush();

//                    if (!expected_to_fail_pp)
//                    {
//                        Assert.IsTrue(r1.Result.ExitCode == 0, $"Exit code : {r1.Result.ExitCode:x}");
//                        Assert.IsTrue(File.Exists(pp_file));
//                        Assert.IsTrue((new FileInfo(pp_file)).Length > 0);
//                    }
//                    else
//                    {
//                        Assert.IsFalse(r1.Result.ExitCode == 0, $"Exit code : {r1.Result.ExitCode:x}");
//                        Console.WriteLine("Preprocessing failed (it was expected)");
//                        return;
//                    }

//                    last_preprocessed_file = Path.Combine(Path.GetDirectoryName(module_src), pp_file);

//                    // Compile

//                    if (ctype == CompilerType.MSVC)
//                    {
//                        compiler_init_cmd = $@"{cc.cobc_bin_dir_path}\..\set_env_vs_{platform}.cmd";
//                    }
//                    Assert.IsTrue(File.Exists(cc.cobc_exe));

//                    Console.WriteLine($"[cobc]: {cc.cobc_exe}");

//                    string outfile = msrc.Replace(".cbl", "." + build_type);

//                    string opt_exe = build_type == "exe" ? "-x" : "";

//                    var r2 = Task.Run(async () =>
//                    {
//                        string cobc_args = $"/C \"{compiler_init_cmd} && {cc.cobc_exe} {opt_exe} -I. -I{cc.gix_copy_path} {pp_file} -l{cc.link_lib_lname} -L{cc.link_lib_dir_path}";
//                        if (additional_cobc_params != "")
//                            cobc_args += (" " + additional_cobc_params);

//                        return await Cli.Wrap("cmd.exe")
//                           .WithArguments(cobc_args)
//                           //.WithStandardOutputPipe(PipeTarget.ToStringBuilder(stdOutBuffer))
//                           //.WithStandardErrorPipe(PipeTarget.ToStringBuilder(stdErrBuffer))
//                           .WithEnvironmentVariables(new Dictionary<string, string>
//                           {
//                               ["COB_CONFIG_DIR"] = cc.cobc_config_dir_path,
//                               ["PATH"] = Environment.GetEnvironmentVariable("PATH") + $";{cc.cobc_bin_dir_path}"
//                           })
//                           .WithValidation(CommandResultValidation.None)
//                           .ExecuteBufferedAsync();
//                    });

//                    Console.WriteLine(r2.Result.StandardOutput);
//                    Console.WriteLine(r2.Result.StandardError);

//                    if (!expected_to_fail_cobc)
//                    {
//                        Assert.IsTrue(r2.Result.ExitCode == 0, $"Exit code : {r2.Result.ExitCode}");
//                        Assert.IsTrue(File.Exists(outfile));
//                        FileInfo fi = new FileInfo(outfile);
//                        Assert.IsTrue(fi.Length > 0);
//                        Console.WriteLine($"Output: {fi.FullName} ({fi.Length} bytes)");
//                    }
//                    else
//                    {
//                        Assert.IsFalse(r2.Result.ExitCode == 0, $"Exit code : {r2.Result.ExitCode}");
//                        Console.WriteLine("COBOL compilation failed (it was expected)");
//                    }

//                    last_compiled_file = outfile;
//                }
//            }
//            finally
//            {
//                Environment.CurrentDirectory = cwd;
//            }
//        }

//        protected void run(CompilerType ctype, string configuration, string platform, string build_type,
//                            string expected_md5_output_hash = "", bool useregex = false, string[] check_output_contains = null, int wait_runtime = 3000)
//        {
//            run1(ctype, configuration, platform, build_type, expected_md5_output_hash, useregex, check_output_contains, wait_runtime);
//        }

//        protected void run1(CompilerType ctype, string configuration, string platform, string build_type,
//                            string expected_md5_output_hash = "", bool useregex = false, string[] check_output_contains = null, int wait_runtime = 3000)
//        {
//            run(0, ctype, configuration, platform, build_type, expected_md5_output_hash, useregex, check_output_contains, wait_runtime);
//        }

//        protected void run2(CompilerType ctype, string configuration, string platform, string build_type,
//                            string expected_md5_output_hash = "", bool useregex = false, string[] check_output_contains = null, int wait_runtime = 3000)
//        {
//            run(1, ctype, configuration, platform, build_type, expected_md5_output_hash, useregex, check_output_contains, wait_runtime);
//        }

//        private void run(int module_idx, CompilerType ctype, string configuration, string platform, string build_type,
//                            string expected_md5_output_hash = "", bool useregex = false, string[] check_output_contains = null, int wait_runtime = 3000)
//        {
//            string cwd = ".";
//            try
//            {
//                Assert.IsTrue(module_idx < module_srcs.Count);
//                string module_src = module_srcs[module_idx];

//                cwd = Environment.CurrentDirectory;
//                Environment.CurrentDirectory = Path.GetDirectoryName(module_src);
//                string module_filename = Path.GetFileName(module_src);

//                string outfile = module_filename.Replace(".cbl", "." + build_type);
//                Assert.IsTrue(File.Exists(outfile));

//                CompilerConfig cc = CompilerConfig.init(ctype, configuration, platform, build_type);

//                string exe = String.Empty;

//                Dictionary<string, string> env = new Dictionary<string, string>();
//                string args = String.Empty;
//                if (build_type == "exe")
//                {
//                    exe = outfile;
//                    env["PATH"] = Environment.GetEnvironmentVariable("PATH") + $";{cc.cobc_bin_dir_path};{cc.link_lib_dir_path}";
//                }
//                else
//                {
//                    exe = cc.cobcrun_exe;
//                    env["PATH"] = Environment.GetEnvironmentVariable("PATH") + $";{cc.cobc_bin_dir_path};{cc.link_lib_dir_path}";
//                    args = module_filename.Substring(0, module_filename.IndexOf("."));
//                }

//                set_db_client_path(platform, env);

//                Console.WriteLine($"Running {exe} {args}");

//                DateTime t1 = DateTime.Now;
                
//                var res = Task.Run(async () =>
//                {
//                    return await Cli.Wrap(exe)
//                        .WithArguments(args)
//                        //.WithStandardOutputPipe(PipeTarget.ToStringBuilder(stdOutBuffer))
//                        //.WithStandardErrorPipe(PipeTarget.ToStringBuilder(stdErrBuffer))
//                        .WithEnvironmentVariables(env)
//                        .WithValidation(CommandResultValidation.None)
//                        .ExecuteBufferedAsync();
//                });

//                Console.WriteLine(res.Result.StandardOutput);
//                Console.WriteLine(res.Result.StandardError);

//                DateTime t2 = DateTime.Now;
//                execution_time = (t2 - t1).TotalMilliseconds;

//                last_error_text = res.Result.StandardError;
//                last_output_text = res.Result.StandardOutput;

//                Assert.IsTrue(res.Result.ExitCode == 0, $"Exit code : {res.Result.ExitCode}");

//                Assert.IsFalse(String.IsNullOrWhiteSpace(res.Result.StandardOutput), "No output");

//                bool b1 = false, b2 = false;

//                if (!String.IsNullOrWhiteSpace(expected_md5_output_hash))
//                {
//                    string out_md5 = Utils.CreateMD5(File.ReadAllBytes(res.Result.StandardOutput));
//                    Assert.AreEqual(expected_md5_output_hash, out_md5, $"Expected: {expected_md5_output_hash}, actual: {out_md5}");
//                    b1 = true;
//                }

//                if (check_output_contains != null && check_output_contains.Length > 0)
//                {
//                    for (int i = 0; i < check_output_contains.Length; i++)
//                    {
//                        string t = check_output_contains[i];
//                        if (useregex)
//                        {
//                            Regex rx = new Regex(t);
//                            Assert.IsTrue(rx.IsMatch(res.Result.StandardOutput), "Output mismatch");
//                        }
//                        else
//                        {
//                            Assert.IsTrue(res.Result.StandardOutput.Contains(t), $"Output mismatch (index: {i}, expected: {t}");
//                        }
//                        b2 = true;
//                    }
//                }

//                if (b1 || b2)
//                    Console.WriteLine("Output: OK");
//                else
//                    Console.WriteLine("WARNING: output not checked");

//                if (__after_run != null)
//                {
//                    __after_run();
//                }
//            }

//            finally
//            {
//                Environment.CurrentDirectory = cwd;
//            }
//        }

//        public void check_file_contains(string filename, string[] check_output_contains, bool useregex = false)
//        {
//            Assert.IsTrue(File.Exists(filename));
//            Assert.IsTrue((new FileInfo(filename)).Length > 0);

//            string content = File.ReadAllText(filename);
//            foreach (string t in check_output_contains)
//            {
//                if (useregex)
//                {
//                    Regex rx = new Regex(t);
//                    Assert.IsTrue(rx.IsMatch(content), "Output mismatch");
//                }
//                else
//                {
//                    Assert.IsTrue(content.Contains(t), $"Output mismatch: missing \"{t}\"");
//                }
//            }
//        }

//        private void set_db_client_path(string platform, Dictionary<string, string> env)
//        {
//            List<string> paths = new List<string>();

//            foreach (var ds in data_sources.Select(a => a.Item1).Distinct())
//            {
//                string v_id = $"{ds.ToUpper()}_CLIENT_PATH_{platform.ToUpper()}";
//                string v_val = Environment.GetEnvironmentVariable(v_id);
//                if (!String.IsNullOrWhiteSpace(v_val))
//                    paths.Add(v_val);
//            }

//            if (paths.Count > 0)
//            {
//                string path = env.ContainsKey("PATH") ? env["PATH"] : String.Empty;
//                path += (";" + String.Join(";", paths));
//                env["PATH"] = path;
//            }

//        }


//        protected string build_data_source_string(bool embed_auth, bool use_port, bool use_opts, int ds_index = 0)
//        {
//            Tuple<string, int> t = data_sources[ds_index];
//            string data_source_type = t.Item1;
//            int data_source_index = t.Item2;

//            string s = data_source_type.ToLower() + "://";

//            if (data_source_type == "odbc")
//            {
//                if (embed_auth)
//                    s += $"{get_ds_val("usr", ds_index)}.{get_ds_val("pwd", ds_index)}@";

//                s += get_ds_val("host", ds_index) + "/";
//            }
//            else
//            {
//                if (embed_auth)
//                    s += $"{get_ds_val("usr", ds_index)}.{get_ds_val("pwd", ds_index)}@";

//                s += get_ds_val("host", ds_index);

//                if (use_port)
//                    s += (":" + get_ds_val("port", ds_index));

//                s += ("/" + get_ds_val("dbname", ds_index));

//            }

//            if (s.EndsWith("/"))
//                s = s.Substring(0, s.Length - 1);

//            if (use_opts)
//            {
//                string o = get_ds_val("opts", ds_index);
//                if (!String.IsNullOrWhiteSpace(o))
//                    s += ("?" + o);
//            }

//            return s;
//        }

//        private string get_ds_val(string base_key, int ds_index = 0)
//        {
//            string k = get_ds_key(base_key, ds_index);
//            return Environment.GetEnvironmentVariable(k);
//        }

//        private string get_ds_key(string key, int ds_index = 0)
//        {
//            Tuple<string, int> t = data_sources[ds_index];
//            string data_source_type = t.Item1;
//            int data_source_index = t.Item2;

//            return $"{data_source_type.ToUpper()}_{data_source_index}_{key.ToUpper()}";
//        }

//        protected string get_datasource_type(int ds_index = 0)
//        {
//            Tuple<string, int> t = data_sources[ds_index];
//            return t.Item1;
//        }

//        protected string get_datasource_host(int ds_index = 0)
//        {
//            return get_ds_val("host", ds_index);
//        }

//        protected string get_datasource_port(int ds_index = 0)
//        {
//            return get_ds_val("port", ds_index);
//        }

//        protected string get_datasource_usr(int ds_index = 0)
//        {
//            return get_ds_val("usr", ds_index);
//        }



//        protected string get_datasource_pwd(int ds_index = 0)
//        {
//            return get_ds_val("pwd", ds_index);
//        }

//        protected string get_datasource_dbname(int ds_index = 0)
//        {
//            return get_ds_val("dbname", ds_index);
//        }
//    }
//}
