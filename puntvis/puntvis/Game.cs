﻿using System;
using System.Collections.Generic;
using System.Linq;

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
		private IReadOnlyCollection<Futures> futures;

		public Game(int punter,
			int punters,
			IEnumerable<Site> sites,
			IEnumerable<River> rivers,
			IEnumerable<Futures> futures)
		{
			this.punter = punter;
			this.punters = punters;
			this.futures = futures.ToArray();
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
		}

		internal void Draw(string destinationFolder)
		{
			DotFormatter.Write(punters,
				sites.Values.ToList(),
				rivers.Values.ToList(),
				futures,
				$"{destinationFolder}/{stateNumber}.dot");
		}

		private static string getEmptyString(int length) =>
			length == 0
				? ""
				: Enumerable.Repeat("_", length)
					.Aggregate((a, b) => a + b);

		internal void Mutate(IReadOnlyCollection<Action> actions)
		{
			foreach (var river in rivers.Values)
				river.ClearSplurge();

			foreach (var claim in actions.OfType<ClaimAction>())
			{
				rivers[new riverKey
				{
					s = claim.source,
					t = claim.target
				}].Claim(claim.punter);
			}
			foreach (var splurge in actions.OfType<SplurgeAction>())
			{
				var prev = splurge.route.First();
				foreach (var next in splurge.route.Skip(1))
				{
					rivers[new riverKey
					{
						s = prev,
						t = next
					}].Claim(splurge.punter, splurge: true);
					prev = next;
				}
			}

			stateNumber++;
		}
	}
}