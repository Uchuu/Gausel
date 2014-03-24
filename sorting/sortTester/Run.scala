package sorttester

/** This is the executable object performing the benchmarking.
  * User defined algorithms should be registered in '''algorithms'''. */
object Run extends sort.SortTester {

  /** Register your algorithms here. */
  val algorithms: List[sort.Sort] = {
    foufou.fakeSort ::
    foufou.fakeRevSort ::
    Nil
  }

  /** Launching benchmarks. */
  run()
}
