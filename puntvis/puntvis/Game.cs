using System;
using System.Collections.Generic;
using System.Linq;
using QuickGraph;
using QuickGraph.Graphviz;
using QuickGraph.Graphviz.Dot;
using System.IO;
using System.Drawing;

namespace puntvis
{
	internal class Game
	{
		private struct riverKey
		{
			public int s;
			public int t;

			public override string ToString()
			{
				return $"({s}->{t})";
			}

			public override int GetHashCode()
			{
				return s.GetHashCode() + t.GetHashCode();
			}

			public override bool Equals(object obj)
			{
				if (obj is riverKey)
				{
					var other = (riverKey)obj;
					return other.s == s && other.t == t
						|| other.t == s && other.s == t;
				}
				else
					return false;
			}
		}

		private Dictionary<riverKey, River> rivers;
		private int punter;
		private int punters;
		private Dictionary<int, Site> sites;

		public int stateNumber = 0;

		UndirectedGraph<Site, River> graph;
		Dictionary<int, Color> palette;

		public Game(int punter, int punters, IEnumerable<Site> sites, IEnumerable<River> rivers)
		{
			this.punter = punter;
			this.punters = punters;
			this.sites = sites.ToDictionary(
				x => x.Id,
				x => x);
			this.rivers = rivers.ToDictionary(
				x => new riverKey
				{
					s = x.Source.Id,
					t = x.Target.Id
				},
				x => x);

			graph = new UndirectedGraph<Site, River>(allowParallelEdges: false);
			graph.AddVertexRange(this.sites.Values);
			graph.AddEdgeRange(this.rivers.Values);

			var r = new Random();
			palette = Enumerable.Range(0, punters)
				.ToDictionary(
					x => x,
					x => x < Palette.colors.Length
						? Palette.colors[x]
						: Color.FromArgb(alpha: 255,
							red: (byte) r.Next(256),
							green: (byte) r.Next(256),
							blue: (byte) r.Next(256)));
		}

		internal void Draw(string destinationFolder)
		{
			var graphviz = new GraphvizAlgorithm<Site, River>(graph);
			graphviz.FormatVertex += (sender, args) =>
			{
				args.VertexFormatter.Label = args.Vertex.Id.ToString();

				args.VertexFormatter.FontColor = args.Vertex is Mine
					? Color.FromArgb(alpha: 255, red: 255, green: 0, blue: 0)
					: Color.FromArgb(alpha: 255, red: 0, green: 0, blue: 255);
			};
			var maxPunterCaptionLength = punters.ToString().Length;
			string empty = getEmptyString(maxPunterCaptionLength);
			graphviz.FormatEdge += (sender, args) =>
			{
				args.EdgeFormatter.Font = new Font(FontFamily.GenericMonospace, 12);
				if (args.Edge.owner == null)
				{
					args.EdgeFormatter.Label.Value = empty;
					args.EdgeFormatter.StrokeColor = Color.Black;
				}
				else
				{
					var label = args.Edge.owner.ToString();
					var diff = maxPunterCaptionLength - label.Length;

					args.EdgeFormatter.Label.Value = getEmptyString(diff) + label;
					args.EdgeFormatter.StrokeColor = palette[args.Edge.owner.Value];
				}
			};

			graphviz.Generate(new FileDotEngine(), $"{destinationFolder}/{stateNumber}.dot");
		}

		private static string getEmptyString(int length) =>
			length == 0
				? ""
				: Enumerable.Repeat("_", length)
					.Aggregate((a, b) => a + b);

		internal void Mutate(IReadOnlyCollection<Action> actions)
		{
			foreach (var claim in actions.OfType<ClaimAction>())
			{
				rivers[new riverKey
				{
					s = claim.source,
					t = claim.target
				}].Claim(claim.punter);
			}
			stateNumber++;
		}
	}

	public class FileDotEngine : IDotEngine
	{
		public string Run(GraphvizImageType imageType, string dot, string outputFileName)
		{
			using (StreamWriter writer = new StreamWriter(outputFileName))
			{
				writer.Write(dot);
			}

			return Path.GetFileName(outputFileName);
		}
	}
}