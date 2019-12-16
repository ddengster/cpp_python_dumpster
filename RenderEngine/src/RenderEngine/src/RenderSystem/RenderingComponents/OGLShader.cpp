
#include "RenderSystem/RenderingComponents/OGLShader.h"
#include "LogManager/LogManager.h"
#include "Math/Vector2.h"
#include "Math/Vector3.h"
#include "Math/Vector4.h"
#include "Math/Matrix4.h"

#define SHADER_ERROR_MSG_MAX_LENGTH 200

DECLARE_ENGINE_NAMESPACE

OGLShader::OGLShader()
:Shader(), mOGLVertexShader(NULL), mOGLPixelShader(NULL)
{
}

void OGLShader::Initialize(	const String& name, const String &path, SHADER_TYPE type,
							SHADER_VERSION version, const String& entry_name,
							const String& data)
{
	Shader::Initialize(name, path, type, version, entry_name);

	String		version_name;
	//todo: can replace with version checks. how to find out: http://ogltotd.blogspot.com/2006/12/checking-for-d3d-shader-version.html
	if (mVersion == OGL_VS || mVersion == OGL_PS)
		version_name = "OGL_SHADER";
	else
		LogManager::GetInstance()->AppendToLog(name + " : Unknown shader version!");

	//int32_t maxLength;
	GLsizei errorlogLength = 0;
	const GLchar* filedata = data.c_str();

	if (mShaderType == VERTEX_SHADER)
	{
		mOGLVertexShader = glCreateShader(GL_VERTEX_SHADER);
		glShaderSource(mOGLVertexShader, 1, &filedata, NULL);
		glCompileShader(mOGLVertexShader);

		//glGetShaderiv(mOGLVertexShader, GL_INFO_LOG_LENGTH, &maxLength);

		char errorLog[SHADER_ERROR_MSG_MAX_LENGTH];
		glGetShaderInfoLog(mOGLVertexShader, SHADER_ERROR_MSG_MAX_LENGTH, &errorlogLength, errorLog);

		if (errorlogLength > 0)
		{
			LogManager::GetInstance()->AppendToLog("\nVertex Shader Compile Info: \n" + name);
			LogManager::GetInstance()->AppendToLog(String(errorLog));
		}
	}
	else //if (mShaderType == PIXEL_SHADER)
	{
		mOGLPixelShader = glCreateShader(GL_FRAGMENT_SHADER);
		glShaderSource(mOGLPixelShader, 1, &filedata, NULL);
		glCompileShader(mOGLPixelShader);

		//glGetShaderiv(mOGLPixelShader, GL_INFO_LOG_LENGTH, &maxLength);

		char errorLog[SHADER_ERROR_MSG_MAX_LENGTH];
		glGetShaderInfoLog(mOGLPixelShader, SHADER_ERROR_MSG_MAX_LENGTH, &errorlogLength, errorLog);

		if (errorlogLength > 0)
		{
			LogManager::GetInstance()->AppendToLog("\nPixel Shader Compile Info: \n" + name);
			LogManager::GetInstance()->AppendToLog(String(errorLog));
		}
	}
}

OGLShader::~OGLShader()
{
	Destroy();
}

void OGLShader::Destroy()
{
	if (mShaderType == VERTEX_SHADER)
		glDeleteShader(mOGLVertexShader);
	else
		glDeleteShader(mOGLPixelShader);
	Shader::Destroy();
}

void OGLShader::SetShader()
{
	//Do nothing, because for ogl the program does the work
}

void OGLShader::UnsetShader()
{
}

void OGLShader::SetFloatByName(const String& name, float v)
{
	//todo, ogl sets shader params on a GPUprogram basis
	glUniform1f(GetSamplerID(name), v);
}

void OGLShader::SetFloat2ByName(const String& name, const Vector2& v2)
{
	//todo
	glUniform2f(GetSamplerID(name), v2.x, v2.y);
}

void OGLShader::SetFloat3ByName(const String& name, const Vector3& v3)
{	
	//todo
	glUniform3f(GetSamplerID(name), v3.x, v3.y, v3.z);
}

void OGLShader::SetFloat4ByName(const String& name, const Vector4& v4)
{
	//todo
	glUniform4f(GetSamplerID(name), v4.x, v4.y, v4.z, v4.w);
}

void OGLShader::SetFloat4x4ByName(const String& name, const Matrix4& m4)
{
	//todo
	glUniformMatrix4fv(GetSamplerID(name), 1, false, m4[0]);
}

int32_t OGLShader::GetSamplerID(const String& name)
{
	//safety check
	if (mProgramAttachedTo == NULL)
		LogManager::GetInstance()->AppendToLog("Shader not attached to any program! "+ name);

	GLint loc;

	OGLGPUProgram *prog = (OGLGPUProgram*)mProgramAttachedTo;
	loc = glGetUniformLocation(prog->mProgram, name.c_str());

	if (loc == -1)
		LogManager::GetInstance()->AppendToLog("No such uniform named: "+ name);

    //printOpenGLError();  // Check for OpenGL errors
    return loc;
}

////////////// OGLGPUProgram //////////////////////

OGLGPUProgram::OGLGPUProgram() 
{
}

OGLGPUProgram::~OGLGPUProgram()
{
}

void OGLGPUProgram::Initialize()
{
	mProgram = glCreateProgram();
	GPUProgram::Initialize();
}

void OGLGPUProgram::AttachShader(Shader* sh)
{
	sh->mProgramAttachedTo = this;
	if (sh->GetShaderType() == VERTEX_SHADER)
	{
		mVertexShader = sh;
		OGLShader* oglshader = (OGLShader*)mVertexShader;
		glAttachShader(mProgram, oglshader->mOGLVertexShader);
	}
	else //if (sh->GetShaderType() == PIXEL_SHADER)
	{
		mPixelShader = sh;
		OGLShader* oglshader = (OGLShader*)mPixelShader;
		glAttachShader(mProgram, oglshader->mOGLPixelShader);
	}
}

void OGLGPUProgram::DetachVertexShader()
{
	OGLShader* oglshader = (OGLShader*)mVertexShader;
	glDetachShader(mProgram, oglshader->mOGLVertexShader);

	GPUProgram::DetachVertexShader();
}

void OGLGPUProgram::DetachPixelShader()
{
	OGLShader* oglshader = (OGLShader*)mPixelShader;
	glDetachShader(mProgram, oglshader->mOGLPixelShader);

	GPUProgram::DetachPixelShader();
}

void OGLGPUProgram::DetachBothShaders()
{
	OGLShader* oglshader = (OGLShader*)mVertexShader;
	glDetachShader(mProgram, oglshader->mOGLVertexShader);
	oglshader = (OGLShader*)mPixelShader;
	glDetachShader(mProgram, oglshader->mOGLPixelShader);

	GPUProgram::DetachBothShaders();
}

void OGLGPUProgram::Bind()
{
	glLinkProgram(mProgram);
}

void OGLGPUProgram::SetGPUProgram()
{
	glUseProgram(mProgram);
}

void OGLGPUProgram::UnsetGPUProgram()
{
	glUseProgram(0);
}

END_ENGINE_NAMESPACE