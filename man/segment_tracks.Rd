% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segmentation_utils.R
\name{segment_tracks}
\alias{segment_tracks}
\title{Segment CTS tracks}
\usage{
segment_tracks(tracks, seg.length, seg.tol)
}
\arguments{
\item{tracks}{df of observations from tracks table (legacy translated from shapefiles)}

\item{seg.length}{numeric desired segment length}

\item{seg.tol}{numeric minimal proportion of \code{seg.length} to be considered a segment}
}
\value{
df segmented tracks
}
\description{
Segment CTS tracks
}
\section{Required Names}{

tracks: (this historically was also created from shapefiles example in raw-data/)
"lat", "long", "transect_id", "dataset_id, "piece", "order".
}

\examples{
\dontrun{
seg <- segment_tracks(tracks, seg.length, seg.tol)
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
  \code{\link{define_lineframe}},
  \code{\link{duplicate_endpoints}},
  \code{\link{listLines}}, \code{\link{obs2Lines}},
  \code{\link{place_midpoints}},
  \code{\link{place_observations}}
}
\concept{nwasc.internals}
