using System;

namespace puntvis
{
	internal class River
	{
		public int? owner { get; private set; }
		public Site Source { get; }
		public Site Target { get; }

		public River(Site source, Site target)
		{
			Source = source;
			Target = target;
		}

		public void Claim(int owner)
		{
			if (this.owner != null)
				throw new InvalidOperationException(
					$"river already claimed. old: {this.owner}, new: {owner}");
			this.owner = owner;
		}
	}
}