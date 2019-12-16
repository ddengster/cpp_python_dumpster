
#include <iostream>
#include "Map2D.h"

using namespace std;

int main()
{
  auto printmap = [](Map2D<char>& map)
  {
    std::cout << "======\n";
    for (int j = map.mHeight - 1; j >= 0; --j)
    {
      for (uint i = 0; i < map.mWidth; ++i)
      {
        std::cout << (uint)map.mData[i + j * map.mWidth] << " ";
      }
      std::cout << std::endl;
    }
    std::cout << "======\n";
  };
  //mouse conv
  {
    Map2D<char> map(128, 128, Vector2I(-64, -64));

    //worldframe.Get(64, 64);
    //limits: [-63,63]
    //s.TempRectangularView();

    Vector2I mouse_coord(1, 1);
    Vector2I array_location = map.ConvertToArrayLoc(mouse_coord);
    std::cout << array_location << std::endl;

    Vector2I worldspaceloc = map.ConvertFromArrayLoc(array_location);
    std::cout << worldspaceloc << std::endl;
  }

  //view generation
  //1) the origin of all generated frames should be wrt the model-space grid
  {
    Map2D<char> map(8, 8, Vector2I(-4, -4));
    map.Blit(0, 0, 1);
    map.WorldSpaceBlit(0, 0, 1);
    map.Blit(6, 7, 1);
    map.WorldSpaceBlit(3, 3, 2);
    map.WorldSpaceBlit(Vector2I(3, 4), 2);

    map.Blit(Vector2I(4, 4), 3); //return: out of range
    printmap(map);
    //auto bad_view = map.TempView(Vector2I(-7, -7), 16, 16); //assert: out of range
    //auto bad_view2 = map.TempView(Vector2I(-9, -9), 4, 4); //assert: out of range

    //Addressing range for new view: 
    //- [0, width/height) useful for algos which are usually applied on [0, width/height)
    //- [worldx, worldy] apply rect patch for world view, but still perform checking
    //- 

    //unlike our parent, this view frame is inclusive of upper right edges
    //auto spt_view = map.WorldSpaceTempView(AABB2D());
    auto spt_view = map.WorldSpaceTempView(Vector2I(-3, -3), 2, 2);
    spt_view.Blit(0, 0, 4);
    spt_view.WorldSpaceBlit(-2, -3, 5); //to the right of 0,0
    //spt_view.Blit(Vector2I(0, 0), 1);
    spt_view.Blit(2, 2, 6); //we can blit at the edge

    auto spt_view2 = map.WorldSpaceTempView(Vector2I(2, -4), 1, 1);
    spt_view2.Blit(0, 0, 7);
    spt_view2.Blit(1, 1, 7);

    printmap(map);
  }

  //patch
  {
    std::cout << "patches\n";
    Map2D<char> map(10, 10, Vector2I(-5, -5));
    
    uint patchw = 3, patchh = 3;
    char* patch = new char[patchw * patchh];
    memset(patch, 1, patchw * patchh);
    
    map.BlitRect(Vector2I(0, 0), patchw, patchh, patch);
    memset(patch, 2, patchw * patchh);
    map.BlitRect(Vector2I(7, 0) + Vector2I(1, 1), patchw, patchh, patch, false);
    //map.BlitRect(Vector2I(8, 0) + Vector2I(1, 1), patchw, patchh, patch, false); // assert: out of range
    memset(patch, 3, patchw * patchh);
    map.WorldSpaceBlitRect(Vector2I(-5, 2), patchw, patchh, patch);
    printmap(map);

    //asserts/returns
    memset(map.mData, 0, map.mWidth * map.mHeight);
    auto spt_view = map.WorldSpaceTempView(Vector2I(-3, -3), 2, 2);
    memset(patch, 1, patchw * patchh);
    spt_view.BlitRect(Vector2I(0, 0), patchw, patchh, patch);
    //spt_view.BlitRect(Vector2I(-1, 0), patchw, patchh, patch); // assert: out of range

    //replace
    memset(patch, 3, patchw * patchh);
    spt_view.WorldSpaceBlitRect(Vector2I(-3, -3), patchw, patchh, patch);

    memset(patch, 2, patchw * patchh);
    //spt_view.BlitRect(Vector2I(1, 1), patchw - 1, patchh - 1, patch); 
    //spt_view.BlitRect(Vector2I(1, 1), patchw, patchh, patch); //assert: out of range
    
    

    //map.BlitRect(Vector2I(-1, -1), patchw, patchh, patch);
    //map.BlitRect(Vector2I(0, 0), patchw, patchh, patch, false);
    //map.BlitRect(Vector2I(15, 15), patchw, patchh, patch, false);
    printmap(map);
  }
  return 0;
}