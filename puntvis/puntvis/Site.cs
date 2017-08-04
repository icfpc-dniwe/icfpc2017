namespace puntvis
{
	internal class Site
	{
		public readonly int Id;
		public readonly double? x;
		public readonly double? y;
		public readonly bool isMine;

		public Site(int id, double? x, double? y, bool isMine)
		{
			Id = id;
			this.x = x;
			this.y = y;
			this.isMine = isMine;
		}
	}
}