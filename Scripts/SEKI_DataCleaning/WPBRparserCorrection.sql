BEGIN;

WITH parsed AS (
    SELECT
        tree_visit_id,
        wpbr_extra_notes,

        substring(
            wpbr_extra_notes
            FROM '(?i)\m([0-9]+)[[:space:]]*ABRC\M'
        )::integer AS active_branch,

        substring(
            wpbr_extra_notes
            FROM '(?i)\m([0-9]+)[[:space:]]*IBRC\M'
        )::integer AS inactive_branch,

        substring(
            wpbr_extra_notes
            FROM '(?i)\m([0-9]+)[[:space:]]*BRC\M'
        )::integer AS unspecified_branch,

        wpbr_active_bole,
        wpbr_inactive_bole,
        wpbr_unspecified_bole,

        wpbr_extra_notes = '4IBC, 1BC' AS special_case

    FROM forest_health.tree_visit
),
corrected AS (
    SELECT
        tree_visit_id,

        CASE WHEN special_case THEN 0
             ELSE active_branch
        END AS active_branch,

        CASE WHEN special_case THEN 4
             ELSE inactive_branch
        END AS inactive_branch,

        unspecified_branch,

        CASE WHEN special_case THEN NULL
             ELSE wpbr_active_bole
        END AS active_bole,

        CASE WHEN special_case THEN NULL
             ELSE wpbr_inactive_bole
        END AS inactive_bole,

        CASE WHEN special_case THEN 1
             ELSE wpbr_unspecified_bole
        END AS unspecified_bole

    FROM parsed
)
UPDATE forest_health.tree_visit AS tv
SET
    wpbr_active_branch = c.active_branch,
    wpbr_inactive_branch = c.inactive_branch,
    wpbr_unspecified_branch = c.unspecified_branch,
    wpbr_active_bole = c.active_bole,
    wpbr_inactive_bole = c.inactive_bole,
    wpbr_unspecified_bole = c.unspecified_bole
FROM corrected AS c
WHERE tv.tree_visit_id = c.tree_visit_id
  AND (
      tv.wpbr_active_branch IS DISTINCT FROM c.active_branch
      OR tv.wpbr_inactive_branch IS DISTINCT FROM c.inactive_branch
      OR tv.wpbr_unspecified_branch IS DISTINCT FROM c.unspecified_branch
      OR tv.wpbr_active_bole IS DISTINCT FROM c.active_bole
      OR tv.wpbr_inactive_bole IS DISTINCT FROM c.inactive_bole
      OR tv.wpbr_unspecified_bole IS DISTINCT FROM c.unspecified_bole
  )
RETURNING
    tv.tree_visit_id,
    tv.wpbr_extra_notes,
    tv.wpbr_active_branch,
    tv.wpbr_inactive_branch,
    tv.wpbr_unspecified_branch,
    tv.wpbr_active_bole,
    tv.wpbr_inactive_bole,
    tv.wpbr_unspecified_bole;

COMMIT;