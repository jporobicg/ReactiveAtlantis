/* FIXTURE ONLY — not copied from Atlantis source.
 * Replace this directory with the real Atlantis codebase via sources.json.
 * Version label: FIXTURE-code-build-X
 */

/* fixture_clearance_rate
 * In this fixture build the clearance coefficient C is applied per second
 * (m3 s-1), then scaled by the time step. The fixture manual describes C as
 * m3 d-1. If both are indexed, the assistant must surface the disagreement
 * rather than pick a winner.
 */
double fixture_clearance_rate(double C, double dt_seconds)
{
    return C * dt_seconds;
}

/* fixture_consume
 * Stand-in of a consumption step: gape filter, pPREY availability, clearance,
 * then a ration cap. Line spans are part of the provenance tests.
 */
double fixture_consume(double prey_biomass,
                       double pprey,
                       double gape_overlap,
                       double C,
                       double dt_seconds,
                       double max_ration)
{
    double available;
    double demand;
    double intake;

    if (gape_overlap <= 0.0) {
        return 0.0;
    }
    available = prey_biomass * pprey * gape_overlap;
    demand = fixture_clearance_rate(C, dt_seconds) * available;
    intake = demand;
    if (intake > max_ration) {
        intake = max_ration;
    }
    return intake;
}
