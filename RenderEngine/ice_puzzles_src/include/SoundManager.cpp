#include "./fmod/fmod.h"

#include "SoundManager.h"

SoundManager SoundManager::sndman;

SoundManager::SoundManager(void)
: initialised(false), pBGMSample(0), bgmChannel(-1), bgmMute(false), m_Muted(false)
{
}

SoundManager::~SoundManager(void)
{
	if (initialised) {
		FSOUND_Close();
	}
}

void SoundManager::Init()
{
	// Initialise fmod library
	if (FSOUND_Init(44100, 32, 0) == 1) {
		initialised = true;
	}
}

void SoundManager::StopSounds()
{
	if (initialised)
		FSOUND_StopSound(FSOUND_ALL);
}

void SoundManager::AdjustMasterVolume(int value)
{
	FSOUND_SetSFXMasterVolume(value);
}

FSOUND_SAMPLE * SoundManager::LoadSound(const std::string& soundfile, bool loop)
{
	if (initialised) {
		SOUNDMAP::iterator it = soundMap.find(soundfile);

		if (it != soundMap.end()) {
			return (*it).second;
		}

		unsigned modeflags = FSOUND_NORMAL;
		if (loop) modeflags |= FSOUND_LOOP_NORMAL;

		FSOUND_SAMPLE * sample = FSOUND_Sample_Load(FSOUND_FREE, soundfile.c_str(), modeflags, 0, 0);
		soundMap.insert(std::make_pair(soundfile, sample));

		return sample;
	}

	return 0;
}

void SoundManager::SetBGM(const std::string& bgmfile)
{
	if (initialised) {
		// Stop previously playing BGM
		if (bgmChannel != -1) {
			FSOUND_StopSound(bgmChannel);
		}

		if (bgmMute == false) {
			pBGMSample = LoadSound(bgmfile, true);
			bgmChannel = FSOUND_PlaySound(bgmChannel, pBGMSample);
		}
	}
}

void SoundManager::SetBGM_Mute(bool turn_on)
{
	if ( turn_on == true ) {
		FSOUND_SetMute(bgmChannel, 0);
		bgmMute = false;
	}
	else if ( turn_on == false ) {
		bgmMute = true;
		FSOUND_SetMute(bgmChannel, 1);
	}
}
void SoundManager::PlaySoundFile(const std::string& soundfile)
{
	if (initialised) {
		FSOUND_SAMPLE * sample = LoadSound(soundfile);

		FSOUND_PlaySound(FSOUND_FREE, sample);
	}
}

void SoundManager::MuteAll()
{
	AdjustMasterVolume(0);
	m_Muted = true;
}

bool SoundManager::IsMuted()
{
	return m_Muted;
}

void SoundManager::UnmuteAll()
{
	AdjustMasterVolume(100);
	m_Muted = true;
}