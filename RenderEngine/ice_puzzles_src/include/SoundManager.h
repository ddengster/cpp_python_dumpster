////////////////////////////////////////////
// SoundManager.h
// Defines a SoundManager class to take care
// of sounds
// July 2006
////////////////////////////////////////////

#ifndef SOUNDMANAGER_H
#define SOUNDMANAGER_H

#include <string>
#include <map>

struct FSOUND_SAMPLE;

class SoundManager
{
public:
	~SoundManager(void);

	static SoundManager * GetInstance() { return &sndman; }

	void Init();
	void StopSounds();
	void AdjustMasterVolume(int value);
	void SetBGM(const std::string& bgmfile);
	void SetBGM_Mute(bool turn_on);
	void PlaySoundFile(const std::string& soundfile);

	FSOUND_SAMPLE * LoadSound(const std::string& soundfile, bool loop=false);

	void MuteAll();
	bool IsMuted();
	void UnmuteAll();
private:
	bool initialised;
	FSOUND_SAMPLE * pBGMSample;
	int bgmChannel;
	bool bgmMute;

	bool m_Muted;

	typedef std::map<std::string, FSOUND_SAMPLE *> SOUNDMAP;
	SOUNDMAP soundMap;

	// For singleton
	static SoundManager sndman;
	// Prevent construction other than one instance
    SoundManager();
    // Prevent copy-construction
    SoundManager(const SoundManager&);
    // Prevent assignment
    SoundManager& operator=(const SoundManager&);
};

#endif//SOUNDMANAGER_H
