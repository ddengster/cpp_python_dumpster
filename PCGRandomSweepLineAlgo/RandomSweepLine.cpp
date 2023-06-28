
#include "DllPCH.h"

#include "DataStructures/priority_queue.h"
#include "DataStructures/devector.h"

#include "Procgen.h"

#include "Sim/SimWorld.h"
#include "Sim/MapInfo.h"

#include "Entities/MinorEnts.h"

#include <random>
#include <algorithm>

ENGINE_NSP



struct ProcgenParams
{
  int   mSeed = 0;
  float mProcgenTimer = 0.0f;
  float mProcgenStepDelay = 0.125f;
  int   mProcgenStepPhase = 0;
  bool  mSkipCoroutine = false;

  SSlotMap<PCGSector> mSectors;
  ProcgenGraph* mDef = nullptr;

  tn::vector<LineSegment2D> mVoronoiEdges;

  struct VoronoiSite
  {
    Vector2 mSitePoint = Vector2::ZERO;
    tn::vector<Vector2> mPolygonPts;
  };
  tn::vector<VoronoiSite> mVoronoiSites;

  void Reset()
  {
    mSectors.Reset();
    mVoronoiEdges.clear();
    mVoronoiSites.clear();
  }

  bool BindAndCheckGraphReqsFulfilled(ProcgenGraph* def);
};


struct PreplacedBox
{
  AABB2D mBox;
  cSpatial* mSpt = nullptr;
};

// Randomize a bunch of PCGSectors
void RandomSweepLineSectorAlgo(tn::vector<PreplacedBox>& preplaced_boxes, AABB2D parentbox, ProcgenParams* params)
{
  struct PreplacedSweepLine
  {
    LineSegment2D mLine;
    cSpatial* mSpt = nullptr;
    AABB2D_LINESEG mSide = LEFT_LINE; //AABB2D_LINESEG_MAX means neither left/right side line
    ObjectHandle mPCGSectorH = NULL_OBJECTHANDLE;
  };
  auto& rng = GetRNG();

  //sweepline left-right, bottom-top algorithm for generating boxes
  tn::vector<PreplacedSweepLine> lines;
  for (uint i = 0; i < preplaced_boxes.size(); ++i)
  {
    for (uint j = i + 1; j < preplaced_boxes.size(); ++j)
    {
      if (preplaced_boxes[i].mBox.IntersectsWithoutTouching(preplaced_boxes[j].mBox))
      {
        LOGC("No boxes should be intersecting!");
        return;
      }
    }

    PCGSector* s = params->mSectors.AddItem();
    s->mBox = preplaced_boxes[i].mBox;
    s->mPreplacedSpt = preplaced_boxes[i].mSpt;

    PreplacedSweepLine l;
    l.mPCGSectorH = s->mHandle;
    l.mLine = preplaced_boxes[i].mBox.LineSeg(LEFT_LINE);
    l.mSpt = preplaced_boxes[i].mSpt;
    l.mSide = LEFT_LINE;
    lines.push_back(l);
    l.mLine = preplaced_boxes[i].mBox.LineSeg(RIGHT_LINE);
    l.mSide = RIGHT_LINE;
    lines.push_back(l);
  }

  auto linesort = [](PreplacedSweepLine& l, PreplacedSweepLine& r) -> bool
  {
    if (l.mLine.mStart.x < r.mLine.mStart.x)
      return true;
    else if (l.mLine.mStart.x == r.mLine.mStart.x)
      return l.mLine.mStart.y < r.mLine.mStart.y;
    return false;
  };
  std::sort(lines.begin(), lines.end(), linesort);

  struct UnFilledBox
  {
    AABB2D mBox;
  };
  tn::vector<UnFilledBox> unfilled;
  UnFilledBox parent;
  parent.mBox = parentbox;
  unfilled.push_back(parent);

  uint cur_idx = 0;
  uint iter = 0;
  do
  {
    /*if (iter == 1)
    {
      return;
    }*/

    //grab all lines with same x
    PreplacedSweepLine& start_ln = lines[cur_idx];
    float cur_sweep_x = start_ln.mLine.mStart.x;
    uint idx_lines_diff_x = cur_idx + 1;
    for (; idx_lines_diff_x < lines.size(); ++idx_lines_diff_x)
    {
      PreplacedSweepLine& ln = lines[idx_lines_diff_x];
      if (cur_sweep_x != ln.mLine.mStart.x)
        break;
    }

    float cur_sweep_y = parentbox.mMin.y;

    //some prep: do slicing in the y-direction. sliced lines represent ranges that you dont build on
    for (uint j = 0; j < preplaced_boxes.size(); ++j)
    {
      PreplacedBox& b = preplaced_boxes[j];
      if (b.mBox.mMin.x < cur_sweep_x && cur_sweep_x < b.mBox.mMax.x)
      {
        PreplacedSweepLine sliced;
        sliced.mLine = LineSegment2D(cur_sweep_x, b.mBox.mMin.y, cur_sweep_x, b.mBox.mMax.y);
        sliced.mSide = AABB2D_LINESEG_MAX;
        sliced.mSpt = b.mSpt;

        PreplacedSweepLine& s_ln = lines[cur_idx];
        if (sliced.mLine.mEnd.y <= s_ln.mLine.mStart.y)
        {
          lines.insert(lines.begin() + cur_idx, sliced);
          cur_sweep_y = sliced.mLine.mEnd.y;
        }
        else
        {
          bool foundinbetween = false;
          uint insertpos = cur_idx, finalln_idx = idx_lines_diff_x - 1;
          for (; insertpos < finalln_idx; ++insertpos)
          {
            PreplacedSweepLine& ln = lines[insertpos];
            PreplacedSweepLine& nextln = lines[insertpos + 1];
            if (ln.mLine.mEnd.y <= sliced.mLine.mStart.y && sliced.mLine.mEnd.y <= nextln.mLine.mStart.y)
            {
              lines.insert(lines.begin() + insertpos, sliced);
              foundinbetween = true;
              break;
            }
          }
          if (!foundinbetween)
            lines.insert(lines.begin() + idx_lines_diff_x, sliced);
        }

        ++idx_lines_diff_x;
        //LOGSS("insert " << sliced.mLine);
      }
    }

    for (uint i = 0; i < unfilled.size(); ++i)
      unfilled[i].mBox.mMax.x = cur_sweep_x;

    //sweep along y and find out what the new unfilled box y-ranges are

    ObjectHandle expanding_sector_h = NULL_OBJECTHANDLE; //current sector expanding upwards
    for (uint i = cur_idx; i < idx_lines_diff_x; ++i)
    {
      PreplacedSweepLine& clipper = lines[i];
      /*for (uint z = 0; z < unfilled.size(); ++z)
      {
        LOGSS(z << " " << unfilled[z].mBox);
      }*/
      //LOGSS(idx);
      struct UnfilledRes
      {
        int offsetIdx = -1;
        bool splitAtClipperStart = false;
        bool splitAtClipperEnd = false;
        bool fullyInternalSector = false;
      };
      int extend_bit = rng.IntRange(0, 0b11);
      //LOGSS(extend_bit);
      /*if (iter == 0)
        extend_bit = 0b00;
      else
       extend_bit = 0b00;*/

      if (clipper.mSide == AABB2D_LINESEG_MAX)
      {
        //vertical line inside a box's x-range
        if (expanding_sector_h != NULL_OBJECTHANDLE)
        {
          //push up the current sector and squash whatever unfilled box there is to the right
          PCGSector* expanding_up_sector = params->mSectors.GetItem(expanding_sector_h);

          bool found_unfilled = false;
          uint j = 0;
          for (; j < unfilled.size(); ++j)
          {
            UnFilledBox& ub = unfilled[j];
            if (ub.mBox.mMin.y == expanding_up_sector->mBox.mMax.y)
            {
              found_unfilled = true;
              break;
            }
          }
          UnFilledBox boxright = unfilled[j]; //retain original box
          if (clipper.mLine.mEnd.y > unfilled[j].mBox.mMax.y)
            unfilled.erase(unfilled.begin() + j); //remove if exceeding
          else
            unfilled[j].mBox.mMin.y = clipper.mLine.mEnd.y; //shift up past the clipperbox's y

          if (found_unfilled)
          {
            boxright.mBox.mMax.y = clipper.mLine.mStart.y; //stop at clipper start
            boxright.mBox.mMin.x = clipper.mLine.mStart.x; //squash to right
            unfilled.insert(unfilled.begin() + j, boxright);
          }

          expanding_up_sector->mBox.mMax.y = clipper.mLine.mStart.y;
          expanding_sector_h = NULL_OBJECTHANDLE;
        }
      }
      else if (clipper.mSide == LEFT_LINE)
      {
        auto getNextUnfilledInRange = [](int offsetidx, float clipper_starty, float clipper_endy,
          tn::vector<UnFilledBox>& unfilled) -> UnfilledRes
        {
          //search for box that overlaps
          UnfilledRes res;
          //LOGSS(clipper_starty);
          for (uint j = (uint)offsetidx; j < unfilled.size(); ++j)
          {
            UnFilledBox& u = unfilled[j];
            if (u.mBox.mMin.y < clipper_starty && u.mBox.mMax.y > clipper_starty)
            {
              res.splitAtClipperStart = true;
              res.offsetIdx = j;
              return res;
            }
            else if (u.mBox.mMin.y >= clipper_starty && u.mBox.mMax.y <= clipper_endy)
            {
              res.fullyInternalSector = true;
              res.offsetIdx = j;
              return res;
            }
            else if (u.mBox.mMin.y < clipper_endy && u.mBox.mMax.y > clipper_endy)
            {
              res.splitAtClipperEnd = true;
              res.offsetIdx = j;
              return res;
            }
          }
          return res;
        };
        UnfilledRes res;
        res.offsetIdx = 0;
        /*LOGSS("asd");
        for (uint z = 0; z < unfilled.size(); ++z)
        {
          LOGSS(z << " " << unfilled[z].mBox);
        }*/
        do
        {
          //expanding_sector_h must be to the left, ie. mMin.x < cur_sweep_x
          res = getNextUnfilledInRange(res.offsetIdx, clipper.mLine.mStart.y, clipper.mLine.mEnd.y, unfilled);
          if (res.offsetIdx != -1)
          {
            if (res.splitAtClipperStart)
            {
              AABB2D box_before_split = unfilled[res.offsetIdx].mBox;
              unfilled[res.offsetIdx].mBox.mMin.y = clipper.mLine.mStart.y; //remaining box after the line

              if (extend_bit & 0b01) //horizontal cap
              {
                UnFilledBox split2; //box to the right
                split2.mBox = box_before_split;

                if (expanding_sector_h != NULL_OBJECTHANDLE)
                {
                  //extend and end the current one
                  PCGSector* expanding_up_sector = params->mSectors.GetItem(expanding_sector_h);
                  expanding_up_sector->mBox.mMax.y = clipper.mLine.mStart.y;
                  expanding_sector_h = NULL_OBJECTHANDLE;

                  split2.mBox.mMin.x = clipper.mLine.mStart.x;
                }

                split2.mBox.mMax.y = clipper.mLine.mStart.y;
                unfilled.insert(unfilled.begin() + res.offsetIdx, split2);
                res.offsetIdx += 1;
              }
              else
              {
                //box cannot be a line for it to be a sector expanding upwards
                if (box_before_split.mMin.x < box_before_split.mMax.x)
                {
                  PCGSector* expanding_up_sector = nullptr;
                  if (expanding_sector_h != NULL_OBJECTHANDLE)
                    expanding_up_sector = params->mSectors.GetItem(expanding_sector_h);
                  else
                  {
                    expanding_sector_h = params->mSectors.AddItemGetHandle();
                    expanding_up_sector = params->mSectors.GetItem(expanding_sector_h);
                    expanding_up_sector->mBox = box_before_split;
                  }
                  expanding_up_sector->mBox.mMax.y = clipper.mLine.mStart.y;

                }
                unfilled[res.offsetIdx].mBox.mMin.y = clipper.mLine.mStart.y;

                UnFilledBox boxright;
                boxright.mBox = box_before_split;
                boxright.mBox.mMin.x = clipper.mLine.mStart.x;
                boxright.mBox.mMax.y = clipper.mLine.mStart.y;
                unfilled.insert(unfilled.begin() + res.offsetIdx, boxright);
                res.offsetIdx += 1;
              }
            }
            else if (res.fullyInternalSector)
            {
              if (unfilled[res.offsetIdx].mBox.Area() > 0.f)
              {
                if (expanding_sector_h != NULL_OBJECTHANDLE)
                {
                  PCGSector* expanding_sector = params->mSectors.GetItem(expanding_sector_h);
                  expanding_sector->mBox.mMax.y = unfilled[res.offsetIdx].mBox.mMax.y;
                  expanding_sector_h = NULL_OBJECTHANDLE;
                }
                else
                {
                  ObjectHandle newsectorh = params->mSectors.AddItemGetHandle();
                  PCGSector* newsector = params->mSectors.GetItem(newsectorh);
                  newsector->mBox = unfilled[res.offsetIdx].mBox;
                }
              }
              unfilled.erase(unfilled.begin() + res.offsetIdx);
            }
            else if (res.splitAtClipperEnd)
            {
              if (extend_bit & 0b10) //horizontal cap
              {
                if (expanding_sector_h != NULL_OBJECTHANDLE)
                {
                  PCGSector* expanding_sector = params->mSectors.GetItem(expanding_sector_h);
                  expanding_sector->mBox.mMax.y = clipper.mLine.mEnd.y;
                  expanding_sector_h = NULL_OBJECTHANDLE;
                }
                else
                {
                  //expanding_sector_h = params->mSectors.AddItemGetHandle();
                  PCGSector* capped_sector = params->mSectors.AddItem();
                  capped_sector->mBox = unfilled[res.offsetIdx].mBox;
                  capped_sector->mBox.mMax.y = clipper.mLine.mEnd.y;
                }

                if (clipper.mLine.mEnd.y < unfilled[res.offsetIdx].mBox.mMax.y)
                  unfilled[res.offsetIdx].mBox.mMin.y = clipper.mLine.mEnd.y;
                else
                  unfilled.erase(unfilled.begin() + res.offsetIdx);
              }
              else //vertical cap
              {
                //box cannot be a line for it to be a sector expanding upwards
                if (unfilled[res.offsetIdx].mBox.mMin.x < unfilled[res.offsetIdx].mBox.mMax.x)
                {
                  PCGSector* expanding_sector = nullptr;
                  if (expanding_sector_h != NULL_OBJECTHANDLE)
                    expanding_sector = params->mSectors.GetItem(expanding_sector_h);
                  else
                  {
                    expanding_sector_h = params->mSectors.AddItemGetHandle();
                    expanding_sector = params->mSectors.GetItem(expanding_sector_h);
                    expanding_sector->mBox = unfilled[res.offsetIdx].mBox;
                  }
                  expanding_sector->mBox.mMax.y = clipper.mLine.mEnd.y;
                }

                if (clipper.mLine.mEnd.y < unfilled[res.offsetIdx].mBox.mMax.y)
                  unfilled[res.offsetIdx].mBox.mMin.y = clipper.mLine.mEnd.y;
                else
                  unfilled.erase(unfilled.begin() + res.offsetIdx);
              }
            }
          }
        } while (res.offsetIdx != -1);
      }
      else //if (clipper.mSide == RIGHT_LINE)
      {
        //search for box whose y ends at start of the clipper line and another at the end
        bool found_start = false, found_end = false;
        uint idx_before_start = 0, idx_after_end = 0;
        for (uint z = 0; z < unfilled.size(); ++z)
        {
          UnFilledBox& ub = unfilled[z];
          if (clipper.mLine.mStart.y == ub.mBox.mMax.y)
          {
            idx_before_start = z;
            found_start = true;
          }
          if (clipper.mLine.mEnd.y == ub.mBox.mMin.y)
          {
            idx_after_end = z;
            found_end = true;
          }
        }

        //look at x-leftlines above or below and determine if they are intersecting, 
        //if make unfilled boxes for the gaps

        params->mClosedRanges.clear();
        //@note: lines never overlap
        bool completelyinside = false;
        for (uint j = cur_idx; j < idx_lines_diff_x; ++j)
        {
          if (j == i)
            continue;

          PreplacedSweepLine& other_leftln = lines[j];
          if (other_leftln.mSide != LEFT_LINE)
            continue;
          if (clipper.mLine.mStart.y >= other_leftln.mLine.mStart.y &&
            clipper.mLine.mEnd.y <= other_leftln.mLine.mEnd.y)
          {
            //completely inside, no need to make any unfilled boxes
            //sweep_y = clipper.mLine.mEnd.y;
            completelyinside = true;
            break;
          }
          if (clipper.mLine.mEnd.y < other_leftln.mLine.mStart.y)
            break; //no longer intersecting, stop
          if (clipper.mLine.mStart.y > other_leftln.mLine.mEnd.y)
            continue; //not within range yet
          params->mClosedRanges.emplace_back(j);
        }

        if (!completelyinside)
        {
          UnFilledBox ub_wholerange;
          uint unfilled_to_subtract_idx = 0;

          if (found_start && !(extend_bit & 0b01))
          {
            //vcap at start
            UnFilledBox& ub = unfilled[idx_before_start];
            if (expanding_sector_h != NULL_OBJECTHANDLE)
            {
              PCGSector* expanding = params->mSectors.GetItem(expanding_sector_h);
              if (ub.mBox.mMin.x == expanding->mBox.mMin.x) //@note: might be a redundant check
                expanding->mBox.mMax.y = ub.mBox.mMax.y;
              expanding_sector_h = NULL_OBJECTHANDLE;
            }
            else
            {
              PCGSector* s = params->mSectors.AddItem();
              s->mBox = ub.mBox;
            }
          }
          if (found_end && !(extend_bit & 0b10))
          {
            //vcap at end
            UnFilledBox& ub = unfilled[idx_after_end];
            PCGSector* s = params->mSectors.AddItem();
            s->mBox = ub.mBox;
          }

          //get a ub which we'll subtract ranges from
          if (extend_bit == 0b11)
          {
            //hcap both ends
            UnFilledBox new_ub;
            new_ub.mBox.mMin.x = new_ub.mBox.mMax.x = cur_sweep_x;
            new_ub.mBox.mMin.y = clipper.mLine.mStart.y;
            new_ub.mBox.mMax.y = clipper.mLine.mEnd.y;
            if (found_end)
            {
              unfilled.insert(unfilled.begin() + idx_after_end, new_ub);
              unfilled_to_subtract_idx = idx_after_end;
            }
            else
            {
              unfilled.push_back(new_ub);
              unfilled_to_subtract_idx = unfilled.size() - 1;
            }
          }
          else if (extend_bit == 0b01)
          {
            //hcap at start, vcap at end
            UnFilledBox new_ub;
            new_ub.mBox.mMin.x = new_ub.mBox.mMax.x = cur_sweep_x;
            new_ub.mBox.mMin.y = clipper.mLine.mStart.y;
            if (found_end)
            {
              new_ub.mBox.mMax.y = unfilled[idx_after_end].mBox.mMax.y;
              //consume ub at idx_after_end
              unfilled[idx_after_end] = new_ub;
              unfilled_to_subtract_idx = idx_after_end;
            }
            else
            {
              new_ub.mBox.mMax.y = clipper.mLine.mEnd.y;
              unfilled.push_back(new_ub);
              unfilled_to_subtract_idx = unfilled.size() - 1;
            }
          }
          else if (extend_bit == 0b10)
          {
            //vcap at start, hcap at end
            UnFilledBox new_ub;
            new_ub.mBox.mMin.x = new_ub.mBox.mMax.x = cur_sweep_x;
            new_ub.mBox.mMax.y = clipper.mLine.mEnd.y;
            if (found_start)
            {
              new_ub.mBox.mMin.y = unfilled[idx_before_start].mBox.mMin.y;
              //consume ub at idx_before_start
              unfilled[idx_before_start] = new_ub;
              unfilled_to_subtract_idx = idx_after_end;
            }
            else
            {
              new_ub.mBox.mMin.y = clipper.mLine.mStart.y;
              unfilled.insert(unfilled.begin(), new_ub);
              unfilled_to_subtract_idx = 0;
            }
          }
          else //if (extend_bit == 0b00) 
          {
            //vcap at both ends
            UnFilledBox new_ub;
            new_ub.mBox.mMin.x = new_ub.mBox.mMax.x = cur_sweep_x;
            if (found_start)
              new_ub.mBox.mMin.y = unfilled[idx_before_start].mBox.mMin.y;
            else
              new_ub.mBox.mMin.y = clipper.mLine.mStart.y;

            if (found_end)
              new_ub.mBox.mMax.y = unfilled[idx_after_end].mBox.mMax.y;
            else
              new_ub.mBox.mMax.y = clipper.mLine.mEnd.y;

            if (found_start && found_end)
            {
              unfilled[idx_before_start] = new_ub;
              unfilled.erase(unfilled.begin() + idx_after_end);
              unfilled_to_subtract_idx = idx_before_start;
            }
            else if (found_start)
            {
              unfilled[idx_before_start] = new_ub;
              unfilled_to_subtract_idx = idx_before_start;
            }
            else if (found_end)
            {
              unfilled[idx_after_end] = new_ub;
              unfilled_to_subtract_idx = idx_after_end;
            }
            else
            {
              unfilled.insert(unfilled.begin(), new_ub);
              unfilled_to_subtract_idx = 0;
            }
          }

          for (uint z = 0; z < params->mClosedRanges.size(); ++z)
          {
            PreplacedSweepLine& closedln = lines[params->mClosedRanges[z]];

            //subtract ranges from full range box
            if (clipper.mLine.mStart.y < closedln.mLine.mStart.y && closedln.mLine.mEnd.y < clipper.mLine.mEnd.y)
            {
              //closedrange completely inside, notouch
              UnFilledBox nextub;
              nextub.mBox.mMin.x = nextub.mBox.mMax.x = cur_sweep_x;
              nextub.mBox.mMin.y = closedln.mLine.mEnd.y;
              nextub.mBox.mMax.y = unfilled[unfilled_to_subtract_idx].mBox.mMax.y;

              //unfilled[unfilled_to_subtract_idx].mBox.mMax.y = closedln.mLine.mStart.y;

              if (nextub.mBox.Area() > 0.f)
              {
                unfilled_to_subtract_idx += 1;
                unfilled.insert(unfilled.begin() + unfilled_to_subtract_idx, nextub);
              }
            }
            else if (closedln.mLine.mStart.y < clipper.mLine.mStart.y)
              unfilled[unfilled_to_subtract_idx].mBox.mMin.y = closedln.mLine.mEnd.y;
            else if (closedln.mLine.mStart.y < clipper.mLine.mEnd.y)
            {
              if (unfilled[unfilled_to_subtract_idx].mBox.mMax.y > closedln.mLine.mEnd.y)
              {
                UnFilledBox remainder_external = unfilled[unfilled_to_subtract_idx];
                remainder_external.mBox.mMin.y = closedln.mLine.mEnd.y;
                unfilled.insert(unfilled.begin() + unfilled_to_subtract_idx + 1, remainder_external);
              }
              unfilled[unfilled_to_subtract_idx].mBox.mMax.y = closedln.mLine.mStart.y;
            }
          }
        }
      }
      cur_sweep_y = clipper.mLine.mEnd.y;
    }

    if (cur_sweep_y < parentbox.mMax.y)
    {
      if (expanding_sector_h != NULL_OBJECTHANDLE)
      {
        PCGSector* expanding_sector = params->mSectors.GetItem(expanding_sector_h);
        //no more clipper lines, fill up the remaining regions
        //find first unfilled after expanding sector 
        uint z = 0;
        float consume_starty = 0.f;
        for (; z < unfilled.size(); ++z)
        {
          UnFilledBox& ub = unfilled[z];
          if (expanding_sector->mBox.mMax.y == ub.mBox.mMin.y)
          {
            consume_starty = expanding_sector->mBox.mMax.y;
            break;
          }
        }

        if (z < unfilled.size())
        {
          UnFilledBox boxright;
          boxright.mBox.mMin.x = boxright.mBox.mMax.x = cur_sweep_x;
          boxright.mBox.mMin.y = consume_starty;
          boxright.mBox.mMax.y = unfilled[z].mBox.mMax.y;
          //start adding sections to the expanding sector
          do
          {
            UnFilledBox& ub = unfilled[z];
            if (ub.mBox.mMin.x == expanding_sector->mBox.mMin.x)
            {
              expanding_sector->mBox.mMax.y = ub.mBox.mMax.y;
              boxright.mBox.mMax.y = ub.mBox.mMax.y;
              unfilled.erase(unfilled.begin() + z);
            }
            else
            {
              //stop expanding
              cur_sweep_y = ub.mBox.mMax.y;
              break;
            }
          } while (z < unfilled.size());
          //LOGSS(boxright.mBox);
          //LOGSS(z);
          unfilled.insert(unfilled.begin() + z, boxright);
          expanding_sector = nullptr;
          expanding_sector_h = NULL_OBJECTHANDLE;
        }

        //further unfilled will carry on, shouldnt need to touch them
        //@todo: cut them off to ensure smaller lengths?
      }
    }

    /*LOGC("after inserts");
    for (uint i = 0; i < unfilled.size(); ++i)
      LOGSS(i << ": " << unfilled[i].mBox);*/
    cur_idx = idx_lines_diff_x;

    ++iter;
    //LOGSS("end iter " << iter);
    /*if (iter == 1)
      break;*/

  } while (cur_idx < lines.size());

  //finish all boxes extending right
  for (uint i = 0; i < unfilled.size(); ++i)
  {
    UnFilledBox& ub = unfilled[i];
    ub.mBox.mMax.x = parentbox.mMax.x;
    if (ub.mBox.Area() == 0.f)
      continue;
    //LOGSS(u.mBox);
    {
      ObjectHandle newsectorh = params->mSectors.AddItemGetHandle();
      PCGSector* newsector = params->mSectors.GetItem(newsectorh);

      newsector->mBox = ub.mBox;
    }
  }
  //return;
  //build adjacencies
  tn::vector<ObjectHandle> picked_largerooms;
  tn::vector<ObjectHandle> selection_generated_largerooms;

  {
    for (uint i = 0; i < params->mSectors.size(); ++i)
    {
      PCGSector* sector1 = params->mSectors[i];
      if (!sector1)
        continue;

      for (uint j = 0; j < params->mSectors.size(); ++j)
      {
        if (i == j)
          continue;
        PCGSector* sector2 = params->mSectors[j];
        if (!sector2)
          continue;
        if (sector1->mBox.SharesEdge(sector2->mBox))
        {
          /*int distreq = rng.IntRange(0, 1);
          if (distreq != 0)
          {
            float dist = sector1.mBox.SharedSegmentShortestDist(sector2.mBox);
            if (dist > 4.f)
              sector1.mAdjacents.push_back(sector2.mHandle);
          }
          else*/
          sector1->mAdjacents.push_back(sector2->mHandle);
          //sector1.mBox.GetSharedEdge(sector2.mBox);
        }
      }

      if (sector1->mBox.SharesEdge(parentbox))
        sector1->mIsBorderRoom = true;
    }
  }

  //do BSP sectioning
  //1) pick an eligible sector based on score
  //2) pick a random axis
  //3) pick a random value for displacement
  //4) split sector into half
  float minwidth = 4.f, minheight = 4.f;
  uint iterations = 0, max_iterations = 4;
  while (iterations < max_iterations)
  {
    ++iterations;
    //LOG("iteration %d", iterations);

    tn::vector<RNGItem<ObjectHandle>> scores;
    for (uint i = 0; i < params->mSectors.size(); ++i)
    {
      auto sector = params->mSectors[i];
      if (!sector)
        continue;
      if (sector->mPreplacedSpt)
        continue; //preplaced

      float area = sector->mBox.Area();
      float width = sector->mBox.GetWidth(), height = sector->mBox.GetHeight();
      if (area > 40.f && width > minwidth && height > minheight)
      {
        int score = (int)(area / 20.f);
        scores.emplace_back(sector->mHandle, score);
      }
    }
    if (scores.empty())
    {
      LOGC("empty");
      continue;
    }

    ObjectHandle h = NULL_OBJECTHANDLE;
    bool res = PickRNGScoredItem(rng, scores, h, false);
    if (!res)
      continue;

    //LOG("selected for splitting: %d", h);
    PCGSector temp_parentsector = *params->mSectors.GetItem(h);
    params->mSectors.RemoveItem(temp_parentsector.mHandle);

    //do splitting
    float width = temp_parentsector.mBox.GetWidth(), height = temp_parentsector.mBox.GetHeight();
    int axis = 0;
    if (width < minwidth)
      axis = 1; //y-axis
    else if (height < minheight)
      axis = 0;
    else
    {
      float factor = (float)width / (float)height;
      if (factor < 1.f)
        factor = 1.f / factor;

      float factorlimit = 1.25f;
      if (width > height && factor > factorlimit)
        axis = 0;
      else if (height > width && factor > factorlimit)
        axis = 1;
      else
        axis = rng.IntRange(0, 1);
    }

    int splitval = 0;
    float percent = rng.FloatRange(0.25f, 0.75f);

    //split along x or y-axis
    if (axis == 0)
      splitval = (int)round(temp_parentsector.mBox.mMin.x + width * percent);
    else
      splitval = (int)round(temp_parentsector.mBox.mMin.y + height * percent);

    ObjectHandle r1h = params->mSectors.AddItemGetHandle();
    ObjectHandle r2h = params->mSectors.AddItemGetHandle();

    PCGSector* r1 = params->mSectors.GetItem(r1h);
    PCGSector* r2 = params->mSectors.GetItem(r2h);

    r1->mBox = temp_parentsector.mBox;
    r2->mBox = temp_parentsector.mBox;
    if (axis == 0)
    {
      r1->mBox.mMax.x = (float)splitval;
      r2->mBox.mMin.x = (float)splitval;
    }
    else //if (axis == 1)
    {
      r1->mBox.mMax.y = (float)splitval;
      r2->mBox.mMin.y = (float)splitval;
    }

    if (r1->mBox.SharesEdge(parentbox))
      r1->mIsBorderRoom = true;
    if (r2->mBox.SharesEdge(parentbox))
      r2->mIsBorderRoom = true;

    for (uint i = 0; i < temp_parentsector.mAdjacents.size(); ++i)
    {
      ObjectHandle adjh = temp_parentsector.mAdjacents[i];
      PCGSector* neighbour = params->mSectors.GetItem(adjh);
      if (!neighbour)
        continue;
      /*if (r2h == 37)
        LOGSS(adjh);*/

      if (neighbour->mBox.SharesEdge(r1->mBox))
      {
        neighbour->RemoveAdjacent(temp_parentsector.mHandle);
        neighbour->mAdjacents.push_back(r1h);
        r1->mAdjacents.push_back(adjh);
      }
      if (neighbour->mBox.SharesEdge(r2->mBox))
      {
        neighbour->RemoveAdjacent(temp_parentsector.mHandle);
        neighbour->mAdjacents.push_back(r2h);
        r2->mAdjacents.push_back(adjh);
      }
    }
    r1->mAdjacents.push_back(r2h);
    r2->mAdjacents.push_back(r1h);
  }
}

void DoGraphFitting(ProcgenGraph* graphdef, ProcgenParams* params, std::unordered_set<PCGSector*>& sectors_to_instantiate)
{
  auto& nodes = graphdef->mPCGGraphNodes;
  auto& rng = GetRNG();

  //@note: technically only builds paths atm
  struct InOuts
  {
    tn::vector<PCGGraphNode*> mStart;
    tn::vector<PCGGraphNode*> mEnd;
    uint mExpectedPathLength = 0;
  };
  std::unordered_map<ObjectHandle, InOuts> pathingnodes;
  for (uint i = 0; i < nodes.size(); ++i)
  {
    PCGGraphNode* node = nodes[i];
    if (!node)
      continue;

#if 0 //@todo
    if (node->mNodeType == ROOM_PATHING)
    {
      //get downstream and upstream nodes
      InOuts inout;
      inout.mStart = graphdef->GetParents(node);
      inout.mEnd = graphdef->GetChildren(node);
      pathingnodes.insert(std::make_pair(node->mHandle, inout));
    }
    else if (node->mL2BindedSector != NULL_OBJECTHANDLE)
    {
      PCGSector* binded_sector = params->mSectors.GetItem(node->mL2BindedSector);
      if (binded_sector)
      {
        binded_sector->mContents = node->mContentSelections;
        sectors_to_instantiate.insert(binded_sector);
      }
    }
#endif
  }

  auto selectPath = [&](PCGGraphNode* startn, PCGGraphNode* endn) -> tn::vector<PCGSector*>
  {
    //do a DFS of all possible paths from start to end and pick a random one

    struct Trail
    {
      PCGSector* mNode = nullptr;
      tn::vector<PCGSector*> mPathToNode;
      uint mBacktracks = 0;
      bool HasDuplicateSector(PCGSector* sector)
      {
        for (uint i = 0; i < mPathToNode.size(); ++i)
        {
          if (mPathToNode[i] == sector)
            return true;
        }
        return false;
      }
      bool IsBackTrackingRelativeToDest(Vector2 start_to_dest_norm, PCGSector* sector)
      {
        Vector2 current_end = sector->mBox.GetCenterPosition();
        int behind_corners = 0;
        for (uint i = 0; i < AABB2D_CORNER_MAX; ++i)
        {
          Vector2 corner = mPathToNode.back()->mBox.GetCorner(BOT_LEFT_CORNER);
          Vector2 current_to_next = current_end - corner;

          float dotp = start_to_dest_norm.DotProduct(current_to_next);
          if (dotp >= 0.0f)
            continue;
          else
            ++behind_corners;
        }
        if (behind_corners == (int)AABB2D_CORNER_MAX)
          return true;
        return false;
      }
    };
    tn::devector<Trail> stack;

    //add initial path
#if 0 //@todo: node refactor
    LOG("path from sector %d to sector %d", startn->mL2BindedSector, endn->mL2BindedSector);
    auto startsector = params->mSectors.GetItem(startn->mL2BindedSector);
    auto endsector = params->mSectors.GetItem(endn->mL2BindedSector);
    if (!startsector)
      LOG("Startsector for PCGGraphNode %s mBindedSpatial %s not found! Make sure mBindedSpatial exists.",
        startn->mName, startn->mBindedSpatial);
    if (!endsector)
    {
      LOG("Endsector for PCGGraphNode %s mBindedSpatial %s not found! Make sure mBindedSpatial exists.",
        endn->mName, endn->mBindedSpatial);
      return tn::vector<PCGSector*>();
    }
#else
    PCGSector* startsector;
    PCGSector* endsector;
#endif

    Vector2 start_to_end = endsector->mBox.GetCenterPosition() - startsector->mBox.GetCenterPosition();
    Vector2 start_to_end_norm = start_to_end.NormalisedCopy();

    Trail t;
    t.mNode = startsector;
    t.mPathToNode.push_back(startsector);
    stack.push_back(t);

    tn::vector<Trail> final_paths;
    while (stack.size())
    {
      Trail cur_trail = stack.back();
      PCGSector* node = cur_trail.mNode;
      stack.pop_back();

      uint stoppathsz = 16;
      if (!node)
        continue;
      if (node == endsector)
      {
        cur_trail.mPathToNode.push_back(endsector);
        final_paths.push_back(cur_trail);
        if (final_paths.size() > 12)
          break;
      }
      else if (cur_trail.mPathToNode.size() >= stoppathsz)
      {
        //discard and continue to next
        continue;
      }
      else
      {
        tn::vector<ObjectHandle> adj = node->mAdjacents;
#pragma warning (push, 0)
#pragma warning (disable:4244)
        std::shuffle(adj.begin(), adj.end(), GetRNG().GetGenerator());
#pragma warning (pop)
        for (uint i = 0; i < adj.size(); ++i)
        {
          PCGSector* n = params->mSectors.GetItem(adj[i]);
          if (!cur_trail.HasDuplicateSector(n))
          {
            bool add_to_trail = false;
            if (cur_trail.IsBackTrackingRelativeToDest(start_to_end_norm, n))
            {
              ++cur_trail.mBacktracks;

              if (cur_trail.mBacktracks < 3)
              {
                uint odds = 4;
                odds += cur_trail.mBacktracks; //decrease likelihood
                int v = GetRNG().IntRange(0, odds - 1);
                if (v == 0)
                  add_to_trail = true;
              }
            }
            else
              add_to_trail = true;

            if (add_to_trail)
            {
              Trail c = cur_trail;
              c.mNode = n;
              c.mPathToNode.push_back(cur_trail.mNode);
              stack.push_back(c);
            }
          }
        }
      }
    }

    tn::vector<PCGSector*> ret;
    if (final_paths.size())
    {
      int idx = rng.IntRange(0, final_paths.size() - 1);
      ret = final_paths[idx].mPathToNode;
      if (ret.size() && endsector != ret.back())
        LOG("path did not end properly at %d", endsector->mHandle);
    }
    return ret;
  };

  for (auto it : pathingnodes)
  {
    it.first;
    InOuts& p = it.second;
    //find all paths comprised of sectors, add to instantiate list
    //@future: can do branching if off a picked path
    for (uint i = 0; i < p.mStart.size(); ++i)
    {
      for (uint j = 0; j < p.mEnd.size(); ++j)
      {
        tn::vector<PCGSector*> path = selectPath(p.mStart[i], p.mEnd[j]);

        for (auto sector : path)
        {
          sectors_to_instantiate.insert(sector);
        }
      }
    }
  }

  //remove adjacents that dont go anywhere
  for (auto sector : sectors_to_instantiate)
  {
    for (uint i = 0; i < sector->mAdjacents.size();)
    {
      ObjectHandle h = sector->mAdjacents[i];
      PCGSector* adj_sector = params->mSectors.GetItem(h);

      if (sectors_to_instantiate.find(adj_sector) == sectors_to_instantiate.end())
        sector->mAdjacents.erase(sector->mAdjacents.begin() + i);
      else
        ++i;
    }
  }
}

END_NSP
