#' Title
#'
#' @param connection db connection
#'
#' @export

mag_sample_occurrences <- function(connection) {
  gopher_query(connection, "
    SELECT
      substr(e.child_uid, instr(e.child_uid, ':') + 1)  AS mag_id,
      substr(e.parent_uid, instr(e.parent_uid, ':') + 1) AS readset_id,
      r.sample_id,
      s.site,
      s.collected_date,
      s.biome,
      meas.value AS moisture,
      meas.unit  AS moisture_unit,
      e.weight   AS rel_abundance
    FROM edge e
    JOIN read_set r
      ON r.readset_id = substr(e.parent_uid, instr(e.parent_uid, ':') + 1)
    JOIN sample s
      ON s.sample_id = r.sample_id
    LEFT JOIN measurement meas
      ON meas.sample_id = s.sample_id
     AND meas.variable = 'moisture'
    WHERE e.edge_type = 'observed_in'
    ORDER BY mag_id, rel_abundance DESC
  ")
}










#' Title
#'
#' @param connection db connection
#' @param mag_id uid of MAG
#'
#' @export

mag_provenance <- function(connection, mag_id) {
  gopher_query(
    connection,
    "
    SELECT
      ? AS mag_id,
      substr(e1.parent_uid, instr(e1.parent_uid, ':') + 1) AS assembly_id,
      substr(e2.parent_uid, instr(e2.parent_uid, ':') + 1) AS readset_id,
      e2.role,
      r.sample_id,
      s.site,
      s.collected_date,
      s.biome
    FROM edge e1
    JOIN edge e2
      ON e2.child_uid = e1.parent_uid
     AND e2.edge_type = 'assembled_from'
    JOIN read_set r
      ON r.readset_id = substr(e2.parent_uid, instr(e2.parent_uid, ':') + 1)
    JOIN sample s
      ON s.sample_id = r.sample_id
    WHERE e1.edge_type = 'binned_from'
      AND e1.child_uid = ?
    ORDER BY assembly_id, readset_id
    ",
    params = list(mag_id, paste0("mag:", mag_id))
  )
}
