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

/* fixture_holling_type_ii
 * Stand-in of the modified Holling Type II grazing term used by
 * Atlantis predcase = 0 (User Guide Part I section 10.2 / Table 18;
 * Murray and Parslow 1997). avail is B* = pPREY * prey * gape.
 * I = C * A / (1 + C/mum * sum(E * avail)); Gr = I * pred_scale * (avail/A).
 */
double fixture_holling_type_ii(double C, double mum, double avail,
                               double E, double pred_scale)
{
    double A;
    double ingest_unit;

    if (avail <= 0.0 || C <= 0.0 || pred_scale <= 0.0) {
        return 0.0;
    }
    A = avail;
    if (mum <= 0.0) {
        ingest_unit = C * A;
    } else {
        ingest_unit = C * A / (1.0 + C * E * A / mum);
    }
    return ingest_unit * pred_scale;
}
