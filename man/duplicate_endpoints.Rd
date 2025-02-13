% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segmentation_utils.R
\name{duplicate_endpoints}
\alias{duplicate_endpoints}
\title{Duplicate Endpoint}
\usage{
duplicate_endpoints(end.pts)
}
\arguments{
\item{end.pts}{df location of segment endpoints}
}
\value{
df of begin and end coordinates of each segment
}
\description{
Duplicate endpoints to create both end and begin coordinates for each segment
}
\examples{
\dontrun{
seg.ends <- duplicate_endpoints(end.pts)
}
}
\seealso{
Other nwasc.internals: \code{\link{assignPointsToLines}},
  \code{\link{calculate_seg_midpoints}},
  \code{\link{calculate_segment_endpoints}},
  \code{\link{complete_segments}},
  \code{\link{create_all_segments}},
  \code{\link{create_empty_segments}},
  \code{\link{create_segmented_df}},
  \code{\link{define_lineframe}}, \code{\link{listLines}},
  \code{\link{obs2Lines}}, \code{\link{place_midpoints}},
  \code{\link{place_observations}},
  \code{\link{segment_tracks}}
}
\concept{nwasc.internals}
