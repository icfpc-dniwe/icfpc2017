using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace puntvis
{
	internal static class DotFormatter
	{
		internal static void Write(
			int punters,
			IReadOnlyCollection<Site> sites,
			IReadOnlyCollection<River> rivers,
			IReadOnlyCollection<Futures> futures,
			string filename)
		{
			var r = new Random();
			var palette = Enumerable.Range(0, punters)
				.ToDictionary(
					x => x,
					x => x < Palette.Colors.Length
						? Palette.Colors[x]
						: getRGBA(
							red: (byte) r.Next(256),
							green: (byte) r.Next(256),
							blue: (byte) r.Next(256)));

			using (var writer = File.CreateText(filename))
			{
				writer.WriteLine("graph G {");
				foreach (var site in sites)
					writer.WriteLine(formatSite(site, futures));

				var maxPunterLength = punters.ToString().Length;
				foreach (var river in rivers)
					writer.WriteLine(formatRiver(river, maxPunterLength, palette));

				writer.WriteLine("}");
			}
		}

		private static string getRGBA(byte red, byte green, byte blue)
			=> $"#{red.ToString("X")}{green.ToString("X")}{blue.ToString("X")}FF";

		private static string formatSite(Site site, IReadOnlyCollection<Futures> futures)
		{
			var label = site.Id.ToString();
			var sf = futures
				.FirstOrDefault(x => x.source == site.Id);
			var tf = futures
				.FirstOrDefault(x => x.target == site.Id);
			if (sf != null)
				label += "↑" + sf.number.ToString();
			if (tf != null)
				label += "↓" + tf.number.ToString();

			var args = new Dictionary<string, string>
			{
				["fontcolor"] = site.isMine ? "#FF0000FF" : "#0000FFFF",
				["label"] = label
			};
			if (site.x != null)
				args["pos"] = $"{site.x},{site.y}!";

			return $"{site.Id} [{formatDictionary(args)}];";
		}

		private static string formatDictionary(Dictionary<string, string> args)
			=> string.Join(" ", args.Select(x => $"{x.Key}=\"{x.Value}\""));

		private static string formatRiver(
			River river,
			int maxPunterCaptionLength,
			Dictionary<int, string> palette)
		{
			var empty = getEmptyString(maxPunterCaptionLength);

			var args = new Dictionary<string, string>
			{
				["fontname"] = "Courier New",
				["fontsize"] = "12"
			};

			if (river.owner == null)
			{
				args["label"] = empty;
				args["color"] = "#000000FF";
			}
			else
			{
				var label = river.owner.ToString();
				var diff = maxPunterCaptionLength - label.Length;

				args["label"] = getEmptyString(diff) + label;
				args["color"] = palette[river.owner.Value];
				if (river.splurge)
					args["style"] = "dashed";
			}

			return $"{river.Source.Id} -- {river.Target.Id} [{formatDictionary(args)}];";
		}

		private static string getEmptyString(int length) =>
			length == 0
				? ""
				: Enumerable.Repeat("_", length)
					.Aggregate((a, b) => a + b);
	}
}