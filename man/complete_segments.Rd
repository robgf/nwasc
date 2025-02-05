% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segmentation_utils.R
\name{complete_segments}
\alias{complete_segments}
\title{Complete Segments}
\usage{
complete_segments(seg.all, seg.ends, seg.min)
}
\arguments{
\item{seg.all}{df of all segments}

\item{seg.ends}{df of begin and end coordinates of each segment}

\item{seg.min}{calculated shortest length of a valid segment}
}
\value{
df completes segments
}
\description{
-Completes the segments by combining endpoints with segment information
-Assigns Segment Ids
}
\section{WARNING!}{
 transects, pieces and segments limited to 10,000 each.
If more segments are required modify the buffer created in the \code{sprintf(} term
}

\examples{
\dontrun{
seg.all.new <- complete_segments(seg.all,seg.ends, seg.min)
}
}
\seealso{
Other nwasc.internals: \code{\link{assignPointsToLines}},
  \code{\link{calculate_seg_midpoints}},
  \code{\link{calculate_segment_endpoints}},
  \code{\link{create_all_segments}},
  \code{\link{create_empty_segments}},
  \code{\link{create_segmented_df}},
  \code{\link{define_lineframe}},
  \code{\link{duplicate_endpoints}},
  \code{\link{listLines}}, \code{\link{obs2Lines}},
  \code{\link{place_midpoints}},
  \code{\link{place_observations}},
  \code{\link{segment_tracks}}
}
\concept{nwasc.internals}
