/* FIXTURE ONLY — not copied from Atlantis source. */

/* fixture_beverton_holt
 * Recruits = alpha * SSB / (beta + SSB).
 * This fixture does not implement larval mortality; a real Atlantis build
 * may apply further survival terms after this curve.
 */
double fixture_beverton_holt(double ssb, double alpha, double beta)
{
    if (beta + ssb <= 0.0) {
        return 0.0;
    }
    return alpha * ssb / (beta + ssb);
}
