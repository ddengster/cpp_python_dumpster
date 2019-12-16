
#include <iostream>
#include <direct.h> //enables _getdrive(); ANSI compatible

#define DISK_SPACE_NEEDED 100000

int main()
{
	//Drive check
	int drive = _getdrive();
	
	std::cout << "Current drive: \n" << (char)(drive + 'A' - 1) << std::endl;

	//Check for amount of disk space on drive
	struct _diskfree_t diskfree;
	_getdiskfree(drive, &diskfree);
	unsigned int neededClusters = DISK_SPACE_NEEDED/(diskfree.sectors_per_cluster*diskfree.bytes_per_sector);

	std::cout << "Available clusters: " << diskfree.avail_clusters << std::endl
		<< "Bytes per sector: " << diskfree.bytes_per_sector << std::endl
		<< "Sectors per cluster: " << diskfree.sectors_per_cluster << std::endl
		<< "Total clusters: " << diskfree.total_clusters << std::endl;

	std::cout << "\nTherefore,\nBytes per cluster: " << (diskfree.bytes_per_sector*diskfree.sectors_per_cluster)<<std::endl;
	if (diskfree.avail_clusters < neededClusters)
	{
		std::cout << "Not enough disk space!\n";
	}
	else
	{
		std::cout << "Enough disk space! Clusters needed: " << neededClusters << std::endl
			<< "which is equal to: " << (diskfree.bytes_per_sector*diskfree.sectors_per_cluster*neededClusters)
			<< " bytes." <<std::endl;
	}

	//Note that neededClusters is an INT, not a float, there 3.??? is rounded down to 3.
	return 0;
}
