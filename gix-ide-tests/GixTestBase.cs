using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;
using System.Threading;
using FlaUI.UIA3;
using FlaUI.Core.AutomationElements;
using FlaUI.Core;
using FlaUI.Core.Conditions;
using FlaUI.Core.Definitions;
using FlaUI.Core.Tools;
using System.Drawing;
using FlaUI.Core.Input;
using Microsoft.Win32;
using System.Text;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace gix_ide_tests
{
    public enum CompilerType
    {
        MSVC,
        MinGW
    }

    public class GixTestBase
    {
        private Window main_window;
        private UIA3Automation automation;
        private Application app;
        protected string TestTempDir;

        private string debug_compiler_id;
        private string release_compiler_id;

        private ConditionFactory cf;

        protected Action __before_build = null;
        protected Action __after_build = null;

        protected Action __before_run = null;
        protected Action __after_run = null;

        protected Action __before_debug = null;
        protected Action __after_debug = null;

        protected bool flat_layout = false;

        [TestInitialize]
        public void Begin()
        {
            Environment.SetEnvironmentVariable("GIX_LOAD_TEST_HELPER", "1");

            System.Attribute[] attrs = System.Attribute.GetCustomAttributes(this.GetType());
            attrs = attrs.Where(a => a is IdeHostPlatformAttribute).ToArray();

            if (attrs.Length != 1)
                Assert.Fail("Test error");

            var pa = (IdeHostPlatformAttribute)attrs[0];
            string platform = pa.Platform.ToUpper();
            string app_base_path = Environment.GetEnvironmentVariable($"APP_BASE_PATH_{platform}");
            string app_path = Path.Combine(app_base_path, "bin", "gix-ide.exe");
            if (!File.Exists(app_path))
                app_path = Path.Combine(app_base_path, "gix-ide.exe");

            if (File.Exists(app_path))
                flat_layout = true;

            string path_var = Environment.GetEnvironmentVariable("PATH");
            path_var = Path.Combine(app_base_path, "bin") + ";" + path_var;

            path_var = Environment.GetEnvironmentVariable($"QT_HOME_{platform}") + "\\bin" + ";" + path_var;

            Environment.SetEnvironmentVariable("PATH", path_var);

            if (Registry.GetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", "DebugCompilerId", null) != null)
                debug_compiler_id = Registry.GetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", "DebugCompilerId", null).ToString();
            
            if (Registry.GetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", "ReleaseCompilerId", null) != null)
                release_compiler_id = Registry.GetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", "ReleaseCompilerId", null).ToString();

            TestTempDir = Path.Combine(Path.GetTempPath(), Utils.RandomString());
            Directory.CreateDirectory(TestTempDir);
            Console.WriteLine($"Test temporary directory: {TestTempDir}");

            automation = new UIA3Automation();
            Console.WriteLine($"Launching {app_path}");
            app = FlaUI.Core.Application.Launch(app_path);
            Thread.Sleep(2000);

            var windows = app.GetAllTopLevelWindows(automation);
            main_window = windows[0];
            cf = automation.ConditionFactory;

            Mouse.MovePixelsPerMillisecond = 100;

            UiHelper.Init();
        }

        [TestCleanup]
        public void End()
        {
            app.Kill();
            Directory.Delete(TestTempDir, true);
            automation.Dispose();

            if (debug_compiler_id != null)
            {
                Registry.SetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", "DebugCompilerId", debug_compiler_id);
            }

            if (release_compiler_id != null)
            {
                Registry.SetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", "ReleaseCompilerId", release_compiler_id);
            }

        }

        protected void set_compiler(CompilerType ctype, string arch)
        {
            string compiler_id = null;
            if (ctype == CompilerType.MSVC)
            {
                compiler_id = Environment.GetEnvironmentVariable("VC_COMPILER_ID");
            }
            else
            {
                if (arch.ToLower() == "x86")
                {
                    compiler_id = Environment.GetEnvironmentVariable("MINGW_COMPILER_X86_ID");
                }

                if (arch.ToLower() == "x64")
                {
                    compiler_id = Environment.GetEnvironmentVariable("MINGW_COMPILER_X64_ID");
                }
            }

            Assert.IsNotNull(compiler_id);

            Registry.SetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", "DebugCompilerId", compiler_id);
            Registry.SetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", "ReleaseCompilerId", compiler_id);

            UiHelper.SetAvailableConfigPlatforms();
        }


        protected void set_build_type(string prj_name, string t)
        {
            var pcw = main_window.FindFirstDescendant(cf.ByName("Project Collection"));
            var tree = pcw.FindFirstDescendant(cf.ByControlType(ControlType.Tree)).AsTree();
            var prj_item = tree.FindFirstDescendant(cf.ByName(prj_name + ".gixprj")).AsTreeItem();

            Point p = prj_item.BoundingRectangle.Center();
            Mouse.MoveTo(new Point((int)p.X, (int)p.Y));
            Mouse.Click(MouseButton.Left);

            var pw = main_window.FindFirstDescendant(cf.ByName("Properties"));
            var ptbl = pw.FindFirstDescendant(cf.ByControlType(ControlType.Table));
            var di = ptbl.FindFirstDescendant(cf.ByName("Build type"));
            p = di.BoundingRectangle.Center();
            Mouse.MoveTo(new Point((int)p.X + 90, (int)p.Y));
            Mouse.Click(MouseButton.Left); Thread.Sleep(200);
            Mouse.MoveTo(new Point((int)p.X + 90, (int)p.Y + (t == "dll" ? 1 : 24)));
            Mouse.Click(MouseButton.Left);
        }

        protected void open_source_file(string prj_name, string src_name)
        {
            var pcw = main_window.FindFirstDescendant(cf.ByName("Project Collection"));
            var tree = pcw.FindFirstDescendant(cf.ByControlType(ControlType.Tree)).AsTree();
            var prj_item = tree.FindFirstDescendant(cf.ByName(prj_name + ".gixprj")).AsTreeItem();
            var src_item = tree.FindFirstDescendant(cf.ByName(src_name)).AsTreeItem();

            Point p = src_item.BoundingRectangle.Center();
            Mouse.MoveTo(new Point((int)p.X, (int)p.Y));
            Mouse.DoubleClick(MouseButton.Left);
        }

        protected string load_project_collection(string prj_name)
        {
            string prjdir = Path.Combine(TestTempDir, Utils.RandomString());
            Directory.CreateDirectory(prjdir);

            Utils.ExtractZipContent("gix_ide_tests.data." + prj_name + ".zip", prjdir);

            var m = main_window.FindFirstByXPath(Utils.ElementPath("fileMenu"));

            m.Click();

            var mm = main_window.FindFirstByXPath(Utils.ElementPath("openProjectMenuItem"));
            mm.Click();

            Thread.Sleep(500);
            var fd = main_window.FindFirstDescendant(cf => cf.ByName("Open Project/Project Collection"));
            Thread.Sleep(500);
            var cb = fd.FindFirstDescendant(cf => cf.ByAutomationId(Utils.ElementId("openFileDialog.Path"))).AsComboBox();
            Thread.Sleep(500);
            var edit = cb.FindFirstDescendant(cf => cf.ByControlType(ControlType.Edit)).AsTextBox();
            Thread.Sleep(500);
            edit.Enter(Path.Combine(prjdir, prj_name + ".gix"));
            Thread.Sleep(500);

            var bok = fd.FindFirstByXPath(Utils.ElementPath("openFileDialog.OK")).AsButton();
            bok.Click();

            Thread.Sleep(1000);

            return prjdir;
        }

        protected void select_configuration_and_platform(string c, string p)
        {
            var cb_configuration = main_window.FindFirstByXPath(Utils.ElementPath("cbConfiguration"));
            var cb_platform = main_window.FindFirstByXPath(Utils.ElementPath("cbPlatform"));

            cb_configuration.Click(); Thread.Sleep(200);
            var li1 = cb_configuration.FindFirstDescendant(cf.ByText(c)).AsListBoxItem();
            li1.Click(); Thread.Sleep(200);

            cb_platform.Click(); Thread.Sleep(200);
            var li2 = cb_platform.FindFirstDescendant(cf.ByText(p)).AsListBoxItem();
            li2.Click(); Thread.Sleep(200);
        }

        protected void build(string prj_name, string prj_dir, string configuration, string platform, string expected_filename)
        {
            if (__before_build != null)
            {
                __before_build();
            }

            string ide_output = Path.Combine(TestTempDir, "ide_output.txt");
            Assert.IsTrue(UiHelper.DuplicateIdeOutput(ide_output));

            var bb = main_window.FindFirstByXPath(Utils.ElementPath("btnBuild"));
            bb.Click();

            string outfile = Path.Combine(prj_dir, prj_name, "bin", configuration.ToLower(), platform.ToLower(), expected_filename);
            RetryResult<bool> b = Retry.WhileFalse(() => File.Exists(outfile), new TimeSpan(0, 0, 10), new TimeSpan(0, 0, 1));
            if (b.Success)
            {
                Console.WriteLine(outfile + ": " + (new FileInfo(outfile)).Length + " bytes");
            }
            else
            {
                string build_log = File.ReadAllText(ide_output);
                Assert.Fail("Build log:\n" + build_log);
            }

            if (__after_build != null)
            {
                __after_build();
            }
        }

        protected void run(string prj_name, string prj_dir, string configuration, string platform, string filename,
                            string expected_md5_output_hash = "", string[] check_output_contains = null, int wait_runtime = 3000)
        {
            if (__before_run != null)
            {
                __before_run();
            }

            string outfile = Path.Combine(prj_dir, prj_name, "bin", configuration.ToLower(), platform.ToLower(), filename);
            string compiler_id = Registry.GetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", configuration + "CompilerId", null).ToString();

            string compiler_def_file = Environment.GetEnvironmentVariable("LOCALAPPDATA") + @"\Gix\compiler-defs\" + compiler_id + ".def";
            Assert.IsTrue(File.Exists(compiler_def_file));

            Assert.IsTrue(File.Exists(outfile) && (outfile.ToLower().EndsWith(".exe") || outfile.ToLower().EndsWith(".dll")));

            string console_out = Path.Combine(TestTempDir, "console_out.txt");
            string console_err = Path.Combine(TestTempDir, "console_err.txt");
            string ide_output = Path.Combine(TestTempDir, "ide_output.txt");

            Assert.IsTrue(UiHelper.DuplicateConsole(console_out, console_err));
            Assert.IsTrue(UiHelper.DuplicateIdeOutput(ide_output));

            var bb = main_window.FindFirstByXPath(Utils.ElementPath("btnRun"));
            bb.Click();

            Thread.Sleep(wait_runtime);

            if (File.Exists(console_out))
            {
                if (!String.IsNullOrWhiteSpace(expected_md5_output_hash))
                {
                    string out_md5 = CreateMD5(File.ReadAllBytes(console_out));
                    Assert.AreEqual(expected_md5_output_hash, out_md5, "Actual output:\n" + File.ReadAllText(console_out) + "\n\nBuild log:\n" + File.ReadAllText(ide_output));

                    // Assert was successful
                    Console.WriteLine("Output: OK");
                }

                if (check_output_contains != null && check_output_contains.Length > 0)
                {
                    foreach (string t in check_output_contains)
                    {
                        Regex rx = new Regex(t);
                        Assert.IsTrue(rx.IsMatch(console_out), "Output does not match" + "\n\nBuild log:\n" + File.ReadAllText(ide_output));

                        // Assert was successful
                        Console.WriteLine("Output: OK");
                    }
                }
            }
            else
            {
                string msg = "\n\nRun error\n============\n\nOutput:\n============\n" + File.ReadAllText(ide_output);
                Assert.Fail(msg);
            }

            string mmsg = "\nConsole output:\n============\n" + File.ReadAllText(console_out);
            Console.WriteLine(mmsg);

            mmsg = "\n\nIDE Output:\n============\n" + File.ReadAllText(ide_output);
            Console.WriteLine(mmsg);

            if (__after_run != null)
            {
                __after_run();
            }
        }

        protected void debug(string prj_name, string prj_dir, string src_file, string configuration, string platform, string filename,
                            List<string> breakpoints, List<string> watched_vars, List<Dictionary<string, string>> expected_values,
                            string expected_md5_output_hash = "", string[] check_output_contains = null, int wait_runtime = 10000)
        {
            if (__before_debug != null)
            {
                __before_debug();
            }

            string outfile = Path.Combine(prj_dir, prj_name, "bin", configuration.ToLower(), platform.ToLower(), filename);
            string compiler_id = Registry.GetValue(@"HKEY_CURRENT_USER\SOFTWARE\MediumGray\gix-ide", configuration + "CompilerId", null).ToString();

            string compiler_def_file = Environment.GetEnvironmentVariable("LOCALAPPDATA") + @"\Gix\compiler-defs\" + compiler_id + ".def";
            Assert.IsTrue(File.Exists(compiler_def_file));

            Assert.IsTrue(File.Exists(outfile) && (outfile.ToLower().EndsWith(".exe") || outfile.ToLower().EndsWith(".dll")));

            string console_out = Path.Combine(TestTempDir, "console_out.txt");
            string console_err = Path.Combine(TestTempDir, "console_err.txt");
            string ide_output = Path.Combine(TestTempDir, "ide_output.txt");

            Assert.IsTrue(UiHelper.DuplicateConsole(console_out, console_err));
            Assert.IsTrue(UiHelper.DuplicateIdeOutput(ide_output));

            UiHelper.AddBreakpoints(breakpoints.ToArray());
            UiHelper.AddWatchedVars(watched_vars.ToArray());

            open_source_file(prj_name, src_file);

            var bb = main_window.FindFirstByXPath(Utils.ElementPath("btnDebug"));
            bb.Click();

            string dbg_src_file = "";
            int dbg_ln = 0;

            RetryResult<bool> debug_start = Retry.WhileFalse(() =>
            {
                return UiHelper.GetDebugStatus(ref dbg_src_file, ref dbg_ln) == IdeStatus.DebuggingOnBreak;

            }, new TimeSpan(10000 * 10000), new TimeSpan(100 * 10000));

            Assert.IsTrue(debug_start.Result, "Debug did not start in the allowed time");

            bool bkp_checked = false;   // we must at least check status on one breakpoint
            int cur_stop = 0;
            RetryResult<bool> not_expired = Retry.WhileTrue(() =>  {

                while (true)
                {
                    // Check if we are stopped at a breakpoint or the process exited
                    

                    RetryResult<bool> breakpoint_reached = Retry.WhileFalse(() => {
                        return UiHelper.GetDebugStatus(ref dbg_src_file, ref dbg_ln) == IdeStatus.DebuggingOnBreak;
                    }, new TimeSpan(5000 * 10000), new TimeSpan(200 * 10000));

                    IdeStatus dbg_status = UiHelper.GetDebugStatus(ref dbg_src_file, ref dbg_ln);

                    if (dbg_status == IdeStatus.DebuggingOnBreak)
                    {
                        string bkp_id = dbg_ln + "@" + dbg_src_file.Replace("/", "\\");
                        Assert.IsTrue(breakpoints.Contains(bkp_id));

                        // If on a breakpoint, check watched variables
                        Dictionary<string, string> wvars_values = UiHelper.GetWatchedVarsValues();
                        Dictionary<string, string> evs = expected_values[cur_stop++];

#if false
                        string e = "";
                        foreach (var kvp in wvars_values) { e += $"{kvp.Key}={kvp.Value.Trim()}; "; }
                        Console.WriteLine($"{cur_stop - 1}: {dbg_ln} : {e}");
#endif
                        foreach (string wvar in watched_vars) {
                            Assert.AreEqual(evs[wvar].Trim(), wvars_values[wvar].Trim(), $"Variable {wvar} -  Expected value: [{evs[wvar].Trim()} - Actual : [{wvars_values[wvar].Trim()}");
                            bkp_checked = true;
                        }

                        // Keep debugging (Run)
                        main_window.Focus();
                        Keyboard.Press(FlaUI.Core.WindowsAPI.VirtualKeyShort.F8);
                        Thread.Sleep(500);

                        continue;
                    }
                    
                    return false;
                }

            }, new TimeSpan(wait_runtime * 10000), new TimeSpan(500 * 10000));

            Assert.IsTrue(not_expired.Result && bkp_checked, "Debug session did not run correctly");

            Thread.Sleep(2000);

            if (File.Exists(console_out))
            {
                if (!String.IsNullOrWhiteSpace(expected_md5_output_hash))
                {
                    string out_md5 = CreateMD5(File.ReadAllBytes(console_out));
                    Assert.AreEqual(expected_md5_output_hash, out_md5, "Actual output:\n" + File.ReadAllText(console_out) + "\n\nBuild log:\n" + File.ReadAllText(ide_output));

                    // Assert was successful
                    Console.WriteLine("Output: OK");
                }

                if (check_output_contains != null && check_output_contains.Length > 0)
                {
                    foreach (string t in check_output_contains)
                    {
                        Regex rx = new Regex(t);
                        Assert.IsTrue(rx.IsMatch(console_out), "Output does not match" + "\n\nBuild log:\n" + File.ReadAllText(ide_output));

                        // Assert was successful
                        Console.WriteLine("Output: OK");
                    }
                }
            }
            else
            {
                string msg = "\n\nRun error\n============\n\nOutput:\n============\n" + File.ReadAllText(ide_output);
                Assert.Fail(msg);
            }

            string mmsg = "\nConsole output:\n============\n" + File.ReadAllText(console_out);
            Console.WriteLine(mmsg);

            mmsg = "\n\nIDE Output:\n============\n" + File.ReadAllText(ide_output);
            Console.WriteLine(mmsg);

            if (__after_debug != null)
            {
                __after_debug();
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

        protected List<string> prepare_breakpoint_list(string prj_dir, string prj_name, string src_file, params int[] lines)
        {
            List<string> res = new List<string>();

            foreach (int line in lines)
            {
                res.Add(line.ToString() + "@" + Path.Combine(prj_dir, prj_name, src_file));
            }
            return res;
        }
    }
}
