using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;

namespace Lab1_UI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private async void Button_Click(object sender, RoutedEventArgs e)
        {
            var isDecryption = sender == Decrypt;

            var text = isDecryption ? tbText.Text : Formatting.textToBinary(tbText.Text);
            var keyText = tbKey.Text;
            string key = await Task.Run(() => RawTextToSHAkey(keyText));            
            tbOutput.Text = await Task.Run(() => GetResult(text, key, isDecryption));
        }

        private string RawTextToSHAkey(string text)
        {
            return String.Concat(System.Security.Cryptography.SHA256.Create().ComputeHash(Encoding.Unicode.GetBytes(text)).Take(64).Select(i => Convert.ToString(i, 2)));
        }       

        private string GetResult(IEnumerable<char> text, string key, bool isDecryption)
        {
            if (isDecryption)
            {
                return new string(Formatting.binaryToText(Cryptography.decryptDES(key, text)).ToArray());
            }
            else
            {
                return Cryptography.encryptDES(key, text);
            }
        }
    }
}
