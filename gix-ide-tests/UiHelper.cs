using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Threading.Tasks;
using System.Net.Sockets;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

namespace gix_ide_tests
{
    enum IdeStatus
    {
        LoadingOrSaving = 0,
	    Started,
	    Editing,
	    Building,
	    Debugging,
	    DebuggingOnBreak,
	    Running,
	    Starting
    };

    static class NSHelper
    {
        public static void WriteLine(this NetworkStream ns, string s)
        {
            s += "\n";
            byte[] bfr = System.Text.Encoding.UTF8.GetBytes(s);
            ns.Write(bfr, 0, bfr.Length);
        }

        public static string ReadMessage(this NetworkStream ns)
        {
            byte[] bfr = new byte[4];
            ns.Read(bfr, 0, 4);
            int len = BitConverter.ToInt32(bfr, 0);
            bfr = new byte[len];
            ns.Read(bfr, 0, len);
            string res = System.Text.Encoding.UTF8.GetString(bfr);
            return res;
        }
    }

    //[TestClass]
    public class UiHelper
    {
        static string host;

        static int port;
        static TcpClient tcp_client;
        static NetworkStream client;
        static string deploy_base;

        public static bool Init(string _host = "localhost", int _port = 19797)
        {
            host = _host;
            port = _port;

            tcp_client = new TcpClient();
            try
            {
                tcp_client.Connect(host, port);
                client = tcp_client.GetStream();
                client.WriteLine("hello");

                string s = client.ReadMessage().Replace("\n", "").Replace("\n", "");
                return (s == "OK");
            }
            catch
            {
                return false;
            }
        }

        public static bool Cleanup(string host, int port)
        {
            return true;
        }

        internal static bool DuplicateConsole(string fout, string ferr)
        {
            client.WriteLine($"dupconsole \"{fout}\" \"{ferr}\"");
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            return s == "OK";
        }

        internal static bool DuplicateIdeOutput(string fideout)
        {
            client.WriteLine($"dupideout \"{fideout}\"");
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            return s == "OK";
        }

        internal static bool SetProjectProperty(string k, string v)
        {
            client.WriteLine($"setprjprop {k} {v}");
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            return s == "OK";
        }

        internal static bool SetProjectProperty(string k, bool b)
        {
            string v = b ? "true" : "false";
            client.WriteLine($"setprjprop {k} {v}");
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            return s == "OK";
        }

        internal static bool SetProjectProperty(string k, string[] vals)
        {
            string v = String.Join("|", vals);
            client.WriteLine($"setprjprop []{k} {v}");
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            return s == "OK";
        }

        internal static bool SetAvailableConfigPlatforms()
        {
            client.WriteLine("setcfgplatforms");
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            return s == "OK";            
        }

        internal static bool AddBreakpoints(string[] brkps)
        {
            client.WriteLine("addbrkps " + String.Join("||", brkps));
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            return s == "OK";
        }

        internal static bool AddWatchedVars(string[] wvars)
        {
            client.WriteLine("addwvars " + String.Join("||", wvars));
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            return s == "OK";
        }

        internal static Dictionary<string, string> GetWatchedVarsValues()
        {
            Dictionary<string, string> res = new Dictionary<string, string>();
            client.WriteLine("getwvars");
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            if (s != "") {
                string[] items = s.Split(new string[] { "||" }, StringSplitOptions.None);
                foreach (string item in items)
                {
                    string name = item.Substring(0, item.IndexOf("="));
                    string val = item.Substring(item.IndexOf("=") + 1);
                    res[name] = val;
                }
            }
            return res;
        }

        internal static IdeStatus GetDebugStatus(ref string dbg_src_file, ref int dbg_ln)
        {
            client.WriteLine("getdbgstatus");
            string s = client.ReadMessage().Replace("\r", "").Replace("\n", "");
            string[] items = s.Split(new string[] { "||" }, StringSplitOptions.None);
            IdeStatus st = (IdeStatus) Int32.Parse(items[0]);
            if (st == IdeStatus.DebuggingOnBreak) {
                string bkp = items[1];
                dbg_ln = Int32.Parse(bkp.Substring(0, bkp.IndexOf("@")));
                dbg_src_file = bkp.Substring(bkp.IndexOf("@") + 1);
            }
            return st;
        }

        //[AssemblyInitialize]
        //public static void Deploy(TestContext ctx)
        //{
        //    deploy_base = Path.Combine(Path.GetTempPath(), Utils.RandomString());
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x86\\bin"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x86\\lib\\copy"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x86\\lib\\x86\\msvc"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x86\\lib\\x86\\gcc"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x86\\lib\\x64\\msvc"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x86\\lib\\x64\\gcc"));

        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x64\\bin"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x64\\lib\\copy"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x64\\lib\\x86\\msvc"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x64\\lib\\x86\\gcc"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x64\\lib\\x64\\msvc"));
        //    Directory.CreateDirectory(Path.Combine(deploy_base, "x64\\lib\\x64\\gcc"));
        //}

        //[AssemblyCleanup]
        //public static void Cleanup(TestContext ctx)
        //{
        //    if (deploy_base != null) {
        //        Directory.Delete(deploy_base, true);
        //    }
        //}
    }
}
