using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using QuickGraph.Algorithms.RandomWalks;

namespace puntvis
{
	class Program
	{
		const string ServerPrefix = "<-";
		const string ClientPrefix = "->";

		static void Main(string[] args)
		{
			if (args.Length < 1)
			{
				Console.WriteLine("usage: 'smh <log>'");
				return;
			}
			var log = args[0];
			if (!File.Exists(log))
			{
				Console.WriteLine($"file '{log}' not found");
				return;
			}
			var destination = log + "-graphviz";
			if (Directory.Exists(destination))
				Directory.Delete(destination, recursive: true);
			Directory.CreateDirectory(destination);

			var states = File.ReadAllLines(log)
				.Where(x => x.StartsWith(ServerPrefix))
				.Skip(1) // you: alice
				.Select(x => x.Substring(ServerPrefix.Length + 1))
				.ToList();

			var rawGame = new
			{
				punter = 0,
				punters = 0,
				map = new
				{
					sites = new[] {new {id = 0}},
					rivers = new[] {new {source = 0, target = 0}},
					mines = new[] {0}
				}
			};
			rawGame = JsonConvert.DeserializeAnonymousType(states.First(), rawGame);
			var mines = new HashSet<int>(rawGame.map.mines);
			var sites = rawGame.map.sites
				.ToDictionary(
					x => x.id,
					x => mines.Contains(x.id) ? new Mine(x.id) : new Site(x.id));
			var game = new Game(
				rawGame.punter,
				rawGame.punters,
				sites.Values,
				rawGame.map.rivers
					.Select(x => new River(sites[x.source], sites[x.target])));
			game.Draw(destination);

			foreach (var jsonDiff in states.Skip(1))
			{
				var rawDiff = new
				{
					move = new
					{
						moves = new[]
						{
							new
							{
								claim = new { punter = 0, source = 0, target = 0 },
								pass = new { punter = 0 }
							}
						}
					},
					stop = new
					{
						moves = new[]
						{
							new
							{
								claim = new { punter = 0, source = 0, target = 0 },
								pass = new { punter = 0 }
							}
						},
						scores = new[] { new {punter = 0, score = 0} }
					}
				};
				rawDiff = JsonConvert.DeserializeAnonymousType(jsonDiff, rawDiff);
				var actions = (rawDiff.move?.moves ?? rawDiff.stop.moves)
					.Select(x =>
					{
						if (x.claim != null)
							return new ClaimAction(x.claim.punter, x.claim.source, x.claim.target) as Action;
						if (x.pass != null)
							return new PassAction(x.pass.punter) as Action;
						throw new InvalidOperationException($"Unable to parse value {x} in {jsonDiff}");
					})
					.ToList();
				game.Mutate(actions);
				game.Draw(destination);
			}
		}
	}
}
