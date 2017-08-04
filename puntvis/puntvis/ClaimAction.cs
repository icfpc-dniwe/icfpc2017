namespace puntvis
{
	internal abstract class Action
	{
		public readonly int punter;

		protected Action(int punter)
		{
			this.punter = punter;
		}
	}

	class PassAction : Action
	{
		public PassAction(int punter) : base(punter)
		{
		}
	}

	internal class ClaimAction: Action
	{
		public readonly  int source;
		public readonly  int target;

		public ClaimAction(int punter, int source, int target) : base(punter)
		{
			this.source = source;
			this.target = target;
		}
	}
}