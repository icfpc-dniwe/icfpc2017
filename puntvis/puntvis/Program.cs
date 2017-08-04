using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace puntvis
{
	class Program
	{
		const string ServerPrefix = "<-";
		const string ClientPrefix = "->";

		static void Main(string[] args)
		{
			if (args.Length < 2)
			{
				Console.WriteLine("you what");
				return;
			}
			var fname = args[1];
			var states = File.ReadAllLines(args[1])
				.Where(x => x.StartsWith(ServerPrefix))
				.Skip(1) // you: alice
				.Select(x => x.Substring(ServerPrefix.Length + 1))
				.ToList();

		}
	}
}
