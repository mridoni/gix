using ICSharpCode.SharpZipLib.Core;
using ICSharpCode.SharpZipLib.Zip;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace gix_ide_tests
{
    internal class Utils
    {
        private static Dictionary<string, string> element_map = new Dictionary<string, string>();

        private static string chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        private static char[] stringChars = new char[8];
        private static Random random = new Random();

        static Utils()
        {
            //var assembly = Assembly.GetExecutingAssembly();
            //var resourceName = "gix_ide_tests.AutomationIDs.txt";

            //using (Stream stream = assembly.GetManifestResourceStream(resourceName))
            //using (StreamReader reader = new StreamReader(stream))
            //{
            //    while (true)
            //    {
            //        string s = reader.ReadLine();
            //        if (s == null)
            //            break;

            //        if (String.IsNullOrWhiteSpace(s) || s.Trim().StartsWith("#") || s.Trim().StartsWith(";") || s.Trim().StartsWith("//") || !s.Contains("="))
            //            continue;

            //        s = s.Trim();

            //        string k = s.Substring(0, s.IndexOf("=")).Trim();
            //        string v = s.Substring(s.IndexOf("=") + 1).Trim();

            //        element_map[k] = v;
            //    }
            //}
        }

        internal static byte[] RandomBytes(int len)
        {
            byte[] data = new byte[len];
            random.NextBytes(data);
            return data;
        }

        public static string ElementPath(string s)
        {
            return element_map[s];
        }

        public static string ElementId(string s)
        {
            return element_map[s].Substring(1);
        }

        public static string RandomString(int length = 10)
        {
            const string chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
            return new string(Enumerable.Repeat(chars, length)
                .Select(s => s[random.Next(s.Length)]).ToArray());
        }

        public static void SaveResource(string resourceName, string outfile)
        {
            var assembly = Assembly.GetExecutingAssembly();
            using (Stream fs = assembly.GetManifestResourceStream("gixsql_tests.data." + resourceName)) {
                using (FileStream o = new FileStream(outfile, FileMode.Create))  {
                    fs.CopyTo(o);
                    fs.Flush();
                }
            }
        }

        public static string GetResource(string resourceName)
        {
            var assembly = Assembly.GetExecutingAssembly();
            using (Stream fs = assembly.GetManifestResourceStream("gixsql_tests.data." + resourceName))
            {
                using (StreamReader sr = new StreamReader(fs)) { 
                    return sr.ReadToEnd();
                }
            }
        }

        public static void ExtractZipContent(string resourceName, string OutputFolder)
        {
            ZipFile file = null;
            try
            {
                //FileStream fs = File.OpenRead(FileZipPath);
                var assembly = Assembly.GetExecutingAssembly();
                using (Stream fs = assembly.GetManifestResourceStream(resourceName))
                {
                    file = new ZipFile(fs);

                    foreach (ZipEntry zipEntry in file)
                    {
                        if (!zipEntry.IsFile)
                        {
                            // Ignore directories
                            continue;
                        }

                        String entryFileName = zipEntry.Name;
                        // to remove the folder from the entry:- entryFileName = Path.GetFileName(entryFileName);
                        // Optionally match entrynames against a selection list here to skip as desired.
                        // The unpacked length is available in the zipEntry.Size property.

                        // 4K is optimum
                        byte[] buffer = new byte[4096];
                        Stream zipStream = file.GetInputStream(zipEntry);

                        // Manipulate the output filename here as desired.
                        String fullZipToPath = Path.Combine(OutputFolder, entryFileName);
                        string directoryName = Path.GetDirectoryName(fullZipToPath);

                        if (directoryName.Length > 0)
                        {
                            Directory.CreateDirectory(directoryName);
                        }

                        // Unzip file in buffered chunks. This is just as fast as unpacking to a buffer the full size
                        // of the file, but does not waste memory.
                        // The "using" will close the stream even if an exception occurs.
                        using (FileStream streamWriter = File.Create(fullZipToPath))
                        {
                            StreamUtils.Copy(zipStream, streamWriter, buffer);
                        }
                    }
                }
            }
            finally
            {
                if (file != null)
                {
                    file.IsStreamOwner = true; // Makes close also shut the underlying stream
                    file.Close(); // Ensure we release resources
                }
            }
        }
    }
}
