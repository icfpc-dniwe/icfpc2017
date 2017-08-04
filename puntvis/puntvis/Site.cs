namespace puntvis
{
	internal class Site
	{
		public readonly int Id;

		public Site(int id)
		{
			Id = id;
		}
	}

	internal class Mine : Site
	{
		public Mine(int id) : base(id)
		{
		}
	}
}