using CliWrap;
using CliWrap.Buffered;
using Microsoft.VisualStudio.TestPlatform.CrossPlatEngine;
//using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace gixsql_tests
{
    [TestFixture]
    //[Ignore]
    public class GixSqlDynamicTestRunner
    {
        private readonly Mutex testMutex = new Mutex(true, "my_mutex_test");
        private string TestTempDir;
        private string cwd;

        static GixSqlDynamicTestRunner()
        {
            DbProviderFactories.RegisterFactory("MySql", MySql.Data.MySqlClient.MySqlClientFactory.Instance);
            DbProviderFactories.RegisterFactory("Npgsql", Npgsql.NpgsqlFactory.Instance);
            DbProviderFactories.RegisterFactory("SQLite", System.Data.SQLite.EF6.SQLiteProviderFactory.Instance);
            DbProviderFactories.RegisterFactory("Odbc", System.Data.Odbc.OdbcFactory.Instance);
            DbProviderFactories.RegisterFactory("Oracle", Oracle.ManagedDataAccess.Client.OracleClientFactory.Instance);
        }
        
        public void Init(GixSqlTestData td)
        {
            testMutex.WaitOne(TimeSpan.FromSeconds(20));

            if (!String.IsNullOrWhiteSpace(Environment.GetEnvironmentVariable("TEST_TEMP_DIR")))
                TestTempDir = Environment.GetEnvironmentVariable("TEST_TEMP_DIR");
            else
                TestTempDir = Path.Combine(Path.GetTempPath(), Utils.RandomString());

            Directory.CreateDirectory(TestTempDir);
            Console.WriteLine($"Test temporary directory: {TestTempDir}");

            cwd = Environment.CurrentDirectory;
            Environment.CurrentDirectory = TestTempDir;

            foreach (List<string> l in td.CobolModules)
            {
                foreach (string d in l)
                {
                    Utils.SaveResource(d, Path.Combine(TestTempDir, d));
                }
            }

        }

        public void Execute(GixSqlTestData td)
        {
            Init(td);

            foreach (var ds in td.DataSources)
            {
                ds.Reset();
            }

            if (td.PreRunDropTable.Count > 0)
            {
                for (int i = 0; i < td.PreRunDropTable.Count; i++)
                {
                    var t = td.PreRunDropTable[i];

                    string client_type = td.DataSources[t.Item1 - 1].type;
                    string sql = "drop table " + t.Item2;

                    // Ignore errors
                    bool b = td.DataSources[t.Item1 - 1].RunSQLStatement(client_type, td.Architecture, sql, null, false);

                }
            }

            if (td.PreRunSQLFile.Count > 0)
            {
                for (int i = 0; i < td.PreRunSQLFile.Count; i++)
                {
                    var t = td.PreRunSQLFile[i];

                    string client_type = td.DataSources[t.Item1 - 1].type;
                    string sql_file = t.Item2;

                    string sql_file_root = Path.GetFileNameWithoutExtension(sql_file);
                    string sql_file_ext = Path.GetExtension(sql_file);

                    string sql = Utils.GetResource(sql_file_root + "-" + client_type + sql_file_ext);
                    if (String.IsNullOrWhiteSpace(sql))
                        sql = Utils.GetResource(sql_file);

                    Assert.IsFalse(String.IsNullOrWhiteSpace(sql));

                    string[] sql_blocks = sql.Split(new string[] { "--" }, StringSplitOptions.RemoveEmptyEntries)
                                                .ToList().ConvertAll(a => a.Trim()).Where(a => !String.IsNullOrWhiteSpace(a)).ToArray();

                    bool b = td.DataSources[i].ExecSQLScript(client_type, td.Architecture, sql_blocks);
                    Assert.IsTrue(b);
                }
            }

            if (td.PreRunSQLStatement.Count > 0)
            {
                for (int i = 0; i < td.PreRunSQLStatement.Count; i++)
                {
                    var t = td.PreRunSQLStatement[i];

                    string client_type = td.DataSources[t.Item1 - 1].type;

                    string stmt_sql = t.Item2.Item1;
                    List<string> stmt_params = t.Item2.Item2;
                    Dictionary<string, string> sp_map = new Dictionary<string, string>();
                    foreach (var p in stmt_params)
                    {
                        Assert.IsTrue(td.GeneratedPayload.ContainsKey(p));
                        sp_map[p] = td.GeneratedPayload[p];
                    }

                    // Ignore errors
                    bool b = td.DataSources[t.Item1 - 1].RunSQLStatement(client_type, td.Architecture, stmt_sql, sp_map);

                }
            }

            if (td.Compile)
            {
                compile(td);
            }

            if (td.Run)
            {
                Dictionary<string, string> resolved_env = new Dictionary<string, string>();
                foreach (var kve in td.Environment)
                {
                    if (!(kve.Value.StartsWith("@{") && kve.Value.EndsWith("}")))
                    {
                        resolved_env[kve.Key] = kve.Value;
                        continue;
                    }

                    string k = "@" + kve.Value.Substring(2);
                    k = k.Substring(0, k.Length - 1);
                    Assert.IsTrue(td.GeneratedPayload.ContainsKey(k));
                    resolved_env[kve.Key] = td.GeneratedPayload[k].Substring(1);
                }
                td.Environment = resolved_env;

                run(td, "", td.ExpectedOutput.ToArray());
            }

        }

        [Test]
        [TestMatrixDataProvider]
        public void GixSqlDynamicTestRunnerInstance(
            [ValueSource(typeof(TestMatrixDataProvider), nameof(TestMatrixDataProvider.GetData))] GixSqlTestData td)
        {
            Execute(td);
        }

        private void compile(GixSqlTestData td)
        {
            string compiler_init_cmd = "break"; // break does nothing

            Assert.IsTrue(td.CobolModules.Count > 0);

            for (int i = 0; i < td.CobolModules.Count; i++)
            {
                string module_src = td.CobolModules[i][0];
                try
                {
                    Assert.IsTrue(File.Exists(module_src));

                    string msrc = Path.GetFileName(module_src);

                    string pp_file = msrc.Replace(".cbl", ".cbsql");

                    CompilerConfig2 cc = td.CompilerConfiguration;

                    // Preprocess
                    string client_pp_params = String.Empty;
                    if (td.DataSources.Count > 0)
                    {
                        string ds_type = td.DataSources[0].type;
                        client_pp_params = TestMatrixDataProvider.GetClientAdditionalPreprocessParams(ds_type, td.Architecture);
                        if (client_pp_params == null)
                            client_pp_params = String.Empty;
                    }
                    string gixpp_args = $"-e -v -S -I. -I{cc.gix_copy_path} -i {msrc} -o {pp_file} {client_pp_params}";
                    if (td.AdditionalPreProcessParams != String.Empty)
                        gixpp_args += (" " + td.AdditionalPreProcessParams);

                    Console.WriteLine($"[gixpp]: {cc.gixpp_exe} {gixpp_args}");

                    var r1 = Task.Run(async () =>
                    {
                        return await Cli.Wrap(cc.gixpp_exe)
                             .WithArguments(gixpp_args)
                             //.WithStandardOutputPipe(PipeTarget.ToStringBuilder(stdOutBuffer, System.Text.Encoding.ASCII))
                             //.WithStandardErrorPipe(PipeTarget.ToStringBuilder(stdErrBuffer, System.Text.Encoding.ASCII))
                             //.WithEnvironmentVariables(env)
                             .WithValidation(CommandResultValidation.None)
                             .ExecuteBufferedAsync();

                    });

                    Console.WriteLine(r1.Result.StandardOutput);
                    Console.WriteLine(r1.Result.StandardError);
                    File.WriteAllText(Path.Combine(TestTempDir, td.Name + "-" + td.Architecture + "-" + (td.DataSources.Count > 0 ? td.DataSources[0].type : "nodata") + "-pp-stdout.log"), r1.Result.StandardOutput);
                    File.WriteAllText(Path.Combine(TestTempDir, td.Name + "-" + td.Architecture + "-" + (td.DataSources.Count > 0 ? td.DataSources[0].type : "nodata") + "-pp-stderr.log"), r1.Result.StandardError);
                    Console.Out.Flush();

                    if (!td.ExpectedToFailPreProcess)
                    {
                        Assert.IsTrue(r1.Result.ExitCode == 0, $"Exit code : {r1.Result.ExitCode:x}");
                        Assert.IsTrue(File.Exists(pp_file));
                        Assert.IsTrue((new FileInfo(pp_file)).Length > 0);
                    }
                    else
                    {
                        Assert.IsFalse(r1.Result.ExitCode == 0, $"Exit code : {r1.Result.ExitCode:x}");
                        Console.WriteLine("Preprocessing failed (it was expected)");
                        return;
                    }

                    td.LastPreprocessedFile = Path.Combine(Path.GetDirectoryName(module_src), pp_file);

                    // check preprocessed (if any)

                    string content = File.ReadAllText(td.LastPreprocessedFile);

                    for (int j = 0; j < td.ExpectedPreprocessedFileContent.Count; j++)
                    {
                        string t = td.ExpectedPreprocessedFileContent[j];
                        bool useregex = t.StartsWith("{{RX}}");
                        if (useregex)
                            t = t.Substring(6);

                        if (useregex)
                        {
                            Regex rx = new Regex(t, RegexOptions.Multiline);
                            Assert.IsTrue(rx.IsMatch(content), "Preprocessed file content mismatch");
                        }
                        else
                        {
                            Assert.IsTrue(content.Contains(t), $"Preprocessed file content mismatch (index: {i}, expected: {t}");
                        }
                    }

                    // Compile

                    if (td.CompilerConfiguration.IsVsBased)
                    {
                        compiler_init_cmd = $@"{cc.cobc_bin_dir_path}\..\set_env_vs_{td.Architecture}.cmd";
                    }
                    Assert.IsTrue(File.Exists(cc.cobc_exe));

                    Console.WriteLine($"[cobc]: {cc.cobc_exe}");

                    string outfile = msrc.Replace(".cbl", "." + td.BuildType);

                    string opt_exe = td.BuildType == "exe" ? "-x" : "";

                    var r2 = Task.Run(async () =>
                    {
                        string cobc_args = $"/C \"{compiler_init_cmd} && {cc.cobc_exe} {opt_exe} -I. -I{cc.gix_copy_path} {pp_file} -L{cc.link_lib_dir_path} -l{cc.link_lib_lname}";
                        Console.WriteLine($"[cobc]: cmd.exe {cobc_args}");


                        if (!td.CompilerConfiguration.IsVsBased)
                        {
                            cobc_args += " -lstdc++ -lfmt";
                        }

                        if (td.AdditionalCompileParams != String.Empty)
                            cobc_args += (" " + td.AdditionalCompileParams);

                        return await Cli.Wrap("cmd.exe")
                           .WithArguments(cobc_args)
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
                    File.WriteAllText(Path.Combine(TestTempDir, td.Name + "-" + td.Architecture + "-" + (td.DataSources.Count > 0 ? td.DataSources[0].type : "nodata") + "-cobc-stdout.log"), r2.Result.StandardOutput);
                    File.WriteAllText(Path.Combine(TestTempDir, td.Name + "-" + td.Architecture + "-" + (td.DataSources.Count > 0 ? td.DataSources[0].type : "nodata") + "-cobc-stderr.log"), r2.Result.StandardError);

                    if (!td.ExpectedToFailCobc)
                    {
                        Assert.IsTrue(r2.Result.ExitCode == 0, $"Exit code : {r2.Result.ExitCode}");
                        Assert.IsTrue(File.Exists(outfile));
                        FileInfo fi = new FileInfo(outfile);
                        Assert.IsTrue(fi.Length > 0);
                        Console.WriteLine($"Output: {fi.FullName} ({fi.Length} bytes)");
                    }
                    else
                    {
                        Assert.IsFalse(r2.Result.ExitCode == 0, $"Exit code : {r2.Result.ExitCode}");
                        Console.WriteLine("COBOL compilation failed (it was expected)");
                    }

                    td.LastCompiledFile = outfile;

                }
                finally
                {

                }
            }
        }

        public void run(GixSqlTestData td, string expected_md5_output_hash = "", string[] check_output_contains = null, int wait_runtime = 3000)
        {
            run1(td, expected_md5_output_hash, check_output_contains, wait_runtime);
        }

        public void run1(GixSqlTestData td, string expected_md5_output_hash = "", string[] check_output_contains = null, int wait_runtime = 3000)
        {
            run(td, 0, expected_md5_output_hash, check_output_contains, wait_runtime);
        }

        public void run2(GixSqlTestData td, string expected_md5_output_hash = "", string[] check_output_contains = null, int wait_runtime = 3000)
        {
            run(td, 1, expected_md5_output_hash, check_output_contains, wait_runtime);
        }

        private void run(GixSqlTestData td, int module_index, string expected_md5_output_hash = "", string[] check_output_contains = null, int wait_runtime = 3000)
        {
            try
            {
                Assert.IsTrue(module_index < td.CobolModules.Count);

                string module_src = td.CobolModules[module_index][0];
                string module_filename = Path.GetFileName(module_src);

                string outfile = module_filename.Replace(".cbl", "." + td.BuildType);
                Assert.IsTrue(File.Exists(outfile));

                CompilerConfig2 cc = td.CompilerConfiguration;

                string exe = String.Empty;

                Dictionary<string, string> env = new Dictionary<string, string>();

                string log_path = Path.Combine(TestTempDir, "gixsql-" + td.Name + ".log");

                if (File.Exists(log_path))
                    File.Delete(log_path);

                env.Add("GIXSQL_LOG_LEVEL", "trace");
                env.Add("GIXSQL_LOG_FILE", log_path);
                foreach (var kve in td.Environment)
                {
                    env.Add(kve.Key, kve.Value);
                }

                if (!env.ContainsKey("PATH"))
                    env["PATH"] = String.Empty;

                string args = String.Empty;
                if (td.BuildType == "exe")
                {
                    exe = outfile;
                    env["PATH"] = env["PATH"] + $";{cc.cobc_bin_dir_path};{cc.link_lib_dir_path}";
                }
                else
                {
                    exe = cc.cobcrun_exe;
                    env["PATH"] = env["PATH"] + $";{cc.cobc_bin_dir_path};{cc.link_lib_dir_path}";
                    args = module_filename.Substring(0, module_filename.IndexOf("."));
                }

                //set_db_client_path(td.Architecture, env);

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
                File.WriteAllText(Path.Combine(TestTempDir, td.Name + "-" + td.Architecture + "-" + (td.DataSources.Count > 0 ? td.DataSources[0].type : "nodata") + "-run-stdout.log"), res.Result.StandardOutput);
                File.WriteAllText(Path.Combine(TestTempDir, td.Name + "-" + td.Architecture + "-" + (td.DataSources.Count > 0 ? td.DataSources[0].type : "nodata") + "-run-stderr.log"), res.Result.StandardError);

                td.LastErrorText = res.Result.StandardError;
                td.LastOutputText = res.Result.StandardOutput;

                Assert.IsTrue(res.Result.ExitCode == 0, $"Exit code : {res.Result.ExitCode}");

                Assert.IsFalse(String.IsNullOrWhiteSpace(res.Result.StandardOutput), "No output");

                bool b1 = false, b2 = false;

                if (!String.IsNullOrWhiteSpace(expected_md5_output_hash))
                {
                    string out_md5 = Utils.CreateMD5(File.ReadAllBytes(res.Result.StandardOutput));
                    Assert.AreEqual(expected_md5_output_hash, out_md5, $"Expected: {expected_md5_output_hash}, actual: {out_md5}");
                    b1 = true;
                }

                Regex rx_subst = new Regex(@"@\{([A-Za-z0-9_]+)\}");
                if (check_output_contains != null && check_output_contains.Length > 0)
                {
                    string content = res.Result.StandardOutput.Replace("\0", "");

                    for (int i = 0; i < check_output_contains.Length; i++)
                    {
                        string t = check_output_contains[i];
                        bool useregex = t.StartsWith("{{RX}}");
                        if (useregex)
                            t = t.Substring(6);

                        if (useregex)
                        {
                            Regex rx = new Regex(t, RegexOptions.Multiline);
                            Assert.IsTrue(rx.IsMatch(content), "Output mismatch");
                        }
                        else
                        {
                            var mc = rx_subst.Matches(t);
                            if (mc.Count > 0)
                            {
                                foreach (Match m in mc)
                                {
                                    string k = "@" + m.Groups[1].Value;
                                    Assert.IsTrue(td.GeneratedPayload.ContainsKey(k));
                                    t = t.Replace("@{" + m.Groups[1].Value + "}", td.GeneratedPayload[k].Substring(1));
                                }
                            }

                            Assert.IsTrue(content.Contains(t), $"Output mismatch (index: {i}, expected: {t}");
                        }
                        b2 = true;
                    }
                }

                if (b1 || b2)
                    Console.WriteLine("Output: OK");
                else
                    Console.WriteLine("WARNING: output not checked");

            }

            finally
            {

            }
        }

        [TearDown]
        public void End()
        {
            testMutex.ReleaseMutex();

            string kt = Environment.GetEnvironmentVariable("KEEP_TEMPS");
            if (kt != "1")
                Directory.Delete(TestTempDir, true);

            Environment.CurrentDirectory = cwd;
        }
    }
}
